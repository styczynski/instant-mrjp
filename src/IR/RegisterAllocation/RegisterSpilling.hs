module IR.RegisterAllocation.RegisterSpilling where

import Control.Lens hiding (Const)

import           Control.Monad.State
import qualified Data.HashMap.Strict                         as HashMap
import qualified Data.HashSet                                as HashSet
import qualified Data.Map                                    as Map
import           IR.Flow.CFG
import           IR.Flow.Liveness
import           IR.Syntax.Syntax
import           IR.Utils
import           IR.RegisterAllocation.InterferenceGraph

data Spill a = Spill {
    _spillSourceCFG :: CFG a ()
    , _spillVarName :: String
    , _spillEstimatedCost    :: Float
}

newtype RegisterSpillageState = St {
    idx :: Integer
}

freshIdx :: State RegisterSpillageState Integer
freshIdx = do
    i <- gets idx
    modify (\st -> st{idx = i + 1})
    return i

spill :: CFG a Liveness -> Integer -> InterferenceNode -> Spill a
spill (CFG g) locN iNode = evalState go (St 0)
    where go = do
            spills <- mapM (spillInBlock (IRValueName $ iNodeLabel iNode) locN) (Map.elems g)
            let combCost = foldr (\n x -> x + costInNode (iNodeLabel iNode) n locN) 0 spills
                combCost' = if combCost == 0 then read "Infinity" else combCost
                g' = Map.fromList $ map (\n -> (n ^. nodeLabel, n)) spills
            return $ Spill (CFG g') (iNodeLabel iNode) (combCost' / fromIntegral (length (iNodeOut iNode)))

spillInBlock :: IRValueName -> Integer -> Node a Liveness -> State RegisterSpillageState (Node a ())
spillInBlock vi locN node = do
    instrs' <- addLoad vi locN (addStore vi locN (node ^. nodeBody))
    return node {_nNodeBody = instrs'}

addStore :: IRValueName -> Integer -> [Instr (a, Liveness)] -> [Instr (a, Liveness)]
addStore _ _ [] = []
addStore vi locN (instr:instrs) =
    let live = snd $ single instr in
    let pos = fst $ single instr
    in case HashMap.lookup (toStr vi) (liveOut live) of
        Just (_, t) | HashSet.member (toStr vi) (liveKill live) ->
            let t' = (pos, emptyLiveness) <$ t
            in  instr:IStore (pos, emptyLiveness) (VVal (pos, emptyLiveness) t' vi) (PLocal (pos, emptyLiveness) t' locN):addStore vi locN instrs
        _ -> instr:addStore vi locN instrs

addLoad :: IRValueName -> Integer -> [Instr (a, Liveness)] -> State RegisterSpillageState [Instr (a, ())]
addLoad _ _ [] = return []
addLoad vi locN (instr:instrs) =
    let live = snd $ single instr
        pos = fst $ single instr
        instr' = (pos, ()) <$ instr
    in case HashMap.lookup (toStr vi) (liveUse live) of
         Just t -> do
             i <- freshIdx
             let vi' = suffix vi locN i
             rest <- addLoad vi locN instrs
             return $ ILoad (pos, ()) vi' (PLocal (pos, ()) (fmap (const $ (pos, ())) t) locN):
                      rename vi vi' instr':rest
         Nothing -> (instr':) <$> addLoad vi locN instrs

costInNode :: String -> Node a d -> Integer -> Float
costInNode vi node locN = foldr (\i x -> instrCost vi i locN + x) 0 (nodeCode node)

rename :: IRValueName -> IRValueName -> Instr a -> Instr a
rename vif vit instr = case instr of
    IRet p v           -> IRet p (f v)
    IOp p vi v1 op v2  -> IOp p vi (f v1) op (f v2)
    ISet p vi v        -> ISet p vi (f v)
    IUnOp p vi op v    -> IUnOp p vi op (f v)
    IVCall p call      -> IVCall p (fc call)
    ICall p vi call    -> ICall p vi (fc call)
    INewArr p vi t v   -> INewArr p vi t (f v)
    ICondJmp p v l1 l2 -> ICondJmp p (f v) l1 l2
    IPhi p vi phiVars  -> IPhi p vi (map fp phiVars)
    ISwap {}           -> error "swap should not occur before phi removal"
    _                  -> instr
    where
        f (VVal p t vi) | vi == vif = VVal p t vit
        f val = val
        fc (Call p t qi vs labs)     = Call p t qi (map f vs) labs
        fc (CallVirt p t qi vs) = CallVirt p t qi (map f vs)
        fp (PhiVar p l v) = PhiVar p l (f v)

instrCost :: String -> Instr a -> Integer -> Float
instrCost _ (ILoad _ _ (PLocal _ _ loc)) locN | loc == locN = 1
instrCost vi (ILoad _ vi' PLocal {}) _ | vi == toStr vi'    = read "Infinity"
instrCost _ _ _ = 0

suffix :: IRValueName -> Integer -> Integer -> IRValueName
suffix (IRValueName vi) n i = IRValueName $ vi ++ "~loc_" ++ show n ++ "_" ++ show i
