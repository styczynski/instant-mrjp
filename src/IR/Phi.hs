module IR.Phi where

import Control.Lens hiding (Const)

import           Data.List
import qualified Data.Map                  as Map
import           Data.Maybe
import qualified Data.Set                  as Set
import           IR.Flow.CFG
import           IR.Flow.Phi
import           IR.Flow.SSA
import           IR.Syntax.Syntax
import           IR.RegisterAllocation.RegisterAllocation

import IR.Utils

import qualified Backend.X64.Parser.Constructor as X64

data JumpRoute = JmpRt LabIdent LabIdent deriving (Eq, Ord)
data DestCfg a = LeaveDest | StoreDest (SType a) (Ptr a)
data SrcCfg a = ValSrc (Val a) | PtrSrc (Ptr a)
data PhiInstr a = PhiInstr ValIdent (DestCfg a) (SrcCfg a)

-- For a given method and its CFG, turn the code into an equivalent
-- version without any IPhi instructions and return the new CFG.
unfoldPhi :: SSA a d -> Method a -> RegisterAllocation -> CFG a ()
unfoldPhi (SSA g) (Mthd pos t qi ps _) rs =
    let (unfolded, jmpRoutes) = unzip $ map go $ lineariseNodes (pos <$ g)
        rewritten = rerouteJumps (concat jmpRoutes) (map (fmap (\(p, _) -> p)) $ concat unfolded)
    in  cfg $ Mthd pos t qi ps rewritten
    where
        go :: (Node a d) -> ([Instr (a, d)], [JumpRoute])
        go node = let l = node ^. nodeLabel
                      code = node ^. nodeBody
                      nontrivial = mapMaybe unfoldTrivialPhi code
                  in createJumpDests rs l nontrivial

-- Unwrap sequences of phi instructions by creating a special block for each incoming
-- label that sets the values specified in the variants for that label and then jumps
-- to the start of the original block. The phi instructions must immediatelly
-- succeed the starting label of the block and its related loads and stores must occur
-- before the endphi instruction.

createJumpDests :: RegisterAllocation -> LabIdent -> [Instr a] -> ([Instr a], [JumpRoute])
createJumpDests _ _ [] = error "internal error. empty node"
createJumpDests rs l (labelInstr:instrs) =
    let pos = single labelInstr
        (phisWithMem, rest) = break isEndPhi instrs
        (phiVars, loadMap, storeMap) = foldr parsePhisWithMem ([], Map.empty, Map.empty) phisWithMem
        (rewrittenPhis, jmpRoutes) = unfoldToJumpDests pos phiVars loadMap storeMap
    in if any isPhi rest
        then error "internal error. phi after IEndPhi"
        else if not $ isLabel labelInstr
        then error "internal error. label not first instruction in node"
        else (rewrittenPhis ++ [labelInstr] ++ rest, jmpRoutes)
    where
        parsePhisWithMem :: Instr a -> ([(ValIdent, [PhiVariant a])], Map.Map ValIdent (Ptr a), Map.Map ValIdent (SType a, Ptr a)) -> ([(ValIdent, [PhiVariant a])], Map.Map ValIdent (Ptr a), Map.Map ValIdent (SType a, Ptr a))
        parsePhisWithMem (IPhi _ vi phiVars) (xs, ld, st)          = ((vi, phiVars):xs, ld, st)
        parsePhisWithMem (ILoad _ vi ptr) (xs, ld, st)             = (xs, Map.insert vi ptr ld, st)
        parsePhisWithMem (IStore _ (VVal _ t vi) ptr) (xs, ld, st) = (xs, ld, Map.insert vi (t, ptr) st)
        parsePhisWithMem _ acc                                     = acc
        unfoldToJumpDests :: a -> [(ValIdent, [PhiVariant a])] -> Map.Map ValIdent (Ptr a) -> Map.Map ValIdent (SType a, Ptr a) -> ([Instr a], [JumpRoute])
        unfoldToJumpDests pos phiVars loadMap storeMap =
            let sourceToValuePairs = Map.toList $ foldr (accVars loadMap storeMap) Map.empty phiVars
                (code, ls) = unzip $ map (createSingleDest pos) sourceToValuePairs
            in (concat code, ls)
        accVars loadMap storeMap (vi, phiVars) acc =
            foldr (accPhiVar loadMap storeMap vi) acc phiVars
        accPhiVar _ _ vi (PhiVar _ _ (VVal _ _ vi')) m | vi == vi' = m
        accPhiVar loadMap storeMap vi (PhiVar _ src v@(VVal _ _ vi')) m =
            let dest = case Map.lookup vi storeMap of
                    Just (t, ptr) -> StoreDest t ptr
                    Nothing       -> LeaveDest
            in case Map.lookup vi' loadMap of
                  Just ptr -> Map.insertWith (++) src [PhiInstr vi dest (PtrSrc ptr)] m
                  Nothing  -> Map.insertWith (++) src [PhiInstr vi dest (ValSrc v)] m
        accPhiVar _ storeMap vi (PhiVar _ src v) m =
            let dest = case Map.lookup vi storeMap of
                    Just (t, ptr) -> StoreDest t ptr
                    Nothing       -> LeaveDest
            in Map.insertWith (++) src [PhiInstr vi dest (ValSrc v)] m
        createSingleDest :: a -> (LabIdent, [PhiInstr a]) -> ([Instr a], JumpRoute)
        createSingleDest pos (lSrc, setVals) =
            (ILabel pos (phiUnfoldJumpFromToLabel lSrc l) :
            generateSetInstructions pos rs setVals
            ++ [IJmp pos l], JmpRt lSrc l)

-- For the routes created by unfolding phi reroute each direct jump from
-- a block to an affected block so that it targets the newly created special
-- labels. So, assuming the same example .L_label as in createJumpDests, the following
-- code:
{-
    .L_source1:
        <code>
        jump .L_label;
    .L_source2:
        <code>
        jump if %v_cond then .L_label else .L_some_other_label;
-}
-- gets rewritten into:
{-
    .L_source1:
        <code>
        jump .L_label__from_source1;
    .L_source2:
        <code>
        jump if %v_cond then .L_label__from_source2 else .L_some_other_label;
-}
rerouteJumps :: [JumpRoute] -> [Instr a] -> [Instr a]
rerouteJumps jmpRoutes instrs = reverse $ fst $ foldl' go ([], entryLabel) instrs
    where
        jumpSet = Set.fromList jmpRoutes
        go :: ([Instr a], LabIdent) -> Instr a -> ([Instr a], LabIdent)
        go (is, lSrc) instr = case instr of
            ILabel _ lSrc' ->
                (instr:is, lSrc')
            ILabelAnn _ lSrc' _ _ ->
                (instr:is, lSrc')
            IJmp pos lDest ->
                 (IJmp pos (reroute lSrc lDest):is, lSrc)
            ICondJmp pos v lDest1 lDest2 ->
                (ICondJmp pos v (reroute lSrc lDest1) (reroute lSrc lDest2):is, lSrc)
            _ ->
                (instr:is, lSrc)
        reroute lSrc lDest =
            if JmpRt lSrc lDest `Set.member` jumpSet
              then phiUnfoldJumpFromToLabel lSrc lDest
              else lDest

isVal :: Val a -> ValIdent -> Bool
isVal (VVal _ _ vi) vi' = vi == vi'
isVal _ _               = False

-- The target registers of values might not form a permutation of the source registers,
-- so a more involved process is required.
-- Assume value from rs has to be transferred to rt.
-- - If we haven't touched rs yet, generate a swap between rt and rs.
-- - Otherwise, save this assignment for later and continue.
-- Additionally, all sets that don't involve two registers (e.g. rt = 42) are postponed.
--
-- After this step we have satisfied a number of phi variants. In particular, if any source
-- register is ever used in any of the variants it has been now swapped with some target register.
-- That means that for every new assignment rt = rs we can lookup rt' with which rs was swapped
-- and emit rt := rt'. All trivial assignments are also emitted now (rt := 42).
generateSetInstructions :: a -> RegisterAllocation -> [PhiInstr a] -> [Instr a]
generateSetInstructions pos rs phis =
    let regMap = Map.fromList (zip X64.allRegs X64.allRegs)
    in  reverse $ go pos [] regMap Map.empty phis []
    where
        go parentPos toSet regMap locked (x@(PhiInstr vi _ src):xs) acc =
            let targetReg = regAlloc rs Map.! vi
            in  case src of
                    ValSrc val ->
                        case val of
                            VVal pos t vi' ->
                                let srcReg = regAlloc rs Map.! vi'
                                in if srcReg `Map.member` locked
                                    then go parentPos (x:toSet) regMap locked xs acc
                                    else let regMap' = Map.insert targetReg srcReg $ Map.insert srcReg targetReg regMap
                                             locked' = Map.insert srcReg vi locked
                                         in go parentPos toSet regMap' locked' xs (ISwap pos t vi vi':acc)
                            _ -> go parentPos (x:toSet) regMap locked xs acc
                    PtrSrc _ ->
                        go parentPos (x:toSet) regMap locked xs acc
        go parentPos (PhiInstr vi dest src:xs) r locked [] acc =
            let store = case dest of
                    StoreDest t ptr -> [IStore parentPos (VVal parentPos t vi) ptr]
                    LeaveDest       -> []
            in
            case src of
                ValSrc val ->
                    case val of
                        VVal pos t vi' ->
                            let srcReg = regAlloc rs Map.! vi'
                                srcVi = locked Map.! srcReg
                            in  go parentPos xs r locked [] (store ++ ISet pos vi (VVal pos t srcVi):acc)
                        _ -> go parentPos xs r locked [] (store ++ ISet pos vi val:acc)
                PtrSrc ptr ->
                    go parentPos xs r locked [] (store ++ ILoad pos vi ptr:acc)
        go _ [] _ _ [] acc = acc


isPhiOrMem :: Instr a -> Bool
isPhiOrMem instr = case instr of
  IPhi {}   -> True
  ILoad {}  -> True
  IStore {} -> True
  _         -> False