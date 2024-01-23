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

data JumpRoute = JmpRt IRLabelName IRLabelName deriving (Eq, Ord)
data DestCfg a = LeaveDest | StoreDest (SType a) (Ptr a)
data SrcCfg a = ValSrc (Val a) | PtrSrc (Ptr a)
data PhiInstr a = PhiInstr IRValueName (DestCfg a) (SrcCfg a)

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


createJumpDests :: RegisterAllocation -> IRLabelName -> [Instr a] -> ([Instr a], [JumpRoute])
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
        parsePhisWithMem :: Instr a -> ([(IRValueName, [PhiVariant a])], Map.Map IRValueName (Ptr a), Map.Map IRValueName (SType a, Ptr a)) -> ([(IRValueName, [PhiVariant a])], Map.Map IRValueName (Ptr a), Map.Map IRValueName (SType a, Ptr a))
        parsePhisWithMem (IPhi _ vi phiVars) (xs, ld, st)          = ((vi, phiVars):xs, ld, st)
        parsePhisWithMem (ILoad _ vi ptr) (xs, ld, st)             = (xs, Map.insert vi ptr ld, st)
        parsePhisWithMem (IStore _ (VVal _ t vi) ptr) (xs, ld, st) = (xs, ld, Map.insert vi (t, ptr) st)
        parsePhisWithMem _ acc                                     = acc
        unfoldToJumpDests :: a -> [(IRValueName, [PhiVariant a])] -> Map.Map IRValueName (Ptr a) -> Map.Map IRValueName (SType a, Ptr a) -> ([Instr a], [JumpRoute])
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
        createSingleDest :: a -> (IRLabelName, [PhiInstr a]) -> ([Instr a], JumpRoute)
        createSingleDest pos (lSrc, setVals) =
            (ILabel pos (phiUnfoldJumpFromToLabel lSrc l) :
            generateSetInstructions pos rs setVals
            ++ [IJmp pos l], JmpRt lSrc l)

rerouteJumps :: [JumpRoute] -> [Instr a] -> [Instr a]
rerouteJumps jmpRoutes instrs = reverse $ fst $ foldl' go ([], entryLabel) instrs
    where
        jumpSet = Set.fromList jmpRoutes
        go :: ([Instr a], IRLabelName) -> Instr a -> ([Instr a], IRLabelName)
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

isVal :: Val a -> IRValueName -> Bool
isVal (VVal _ _ vi) vi' = vi == vi'
isVal _ _               = False

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