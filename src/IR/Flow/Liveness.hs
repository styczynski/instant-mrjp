{-# LANGUAGE TupleSections #-}
-- Annotations of variable liveness for CFGs.
-- This is done multiple times during code generation, including in a loop
-- in optimisation pipeline, so hashed containers are used to amp the performance.
module IR.Flow.Liveness where

import Control.Lens hiding (Const)

import           Data.Bifunctor
import qualified Data.HashMap.Strict      as Map
import qualified Data.HashSet             as Set
import qualified Data.Map                 as OrdMap
import qualified Data.Set                 as OrdSet
import           IR.Flow.CFG
import           IR.Syntax.Syntax
import IR.Utils


type VarSet = Set.HashSet String
type NextUse = Map.HashMap String (Int, SType ())
type TypedVarSet = Map.HashMap String (SType ())

data Liveness = Liveness {
    liveIn   :: NextUse,
    liveOut  :: NextUse,
    liveUse  :: TypedVarSet,
    liveKill :: VarSet
} deriving Eq

emptyLiveness :: Liveness
emptyLiveness = Liveness Map.empty Map.empty Map.empty Set.empty

analyseLiveness :: CFG a d -> CFG a Liveness
analyseLiveness g = let start = initialLiveness g
                    in  fixpoint iterateLiveness start

initialLiveness :: CFG a d -> CFG a Liveness
initialLiveness = linearMap nodeInitLive
    where
          nodeInitLive :: Node a d -> Node a Liveness
          nodeInitLive node = node { _nNodeBody = map instrInitLive (node ^. nodeBody) }
          instrInitLive :: Instr (a, b) -> Instr (a, Liveness)
          instrInitLive instr =
              let (use, kill) = case instr of
                    IRet _ v          -> (valSet v, Set.empty)
                    IOp _ vi v1 _ v2  -> (valsSet [v1, v2], valISet vi)
                    ISet _ vi v       -> (valSet v, valISet vi)
                    ISwap _ t vi1 vi2 -> (Map.fromList [(toStr vi2, () <$ t)], valISet vi1)
                    IUnOp _ vi _ v    -> (valSet v, valISet vi)
                    IVCall _ call     -> (callUseSet call, Set.empty)
                    ICall _ vi call   -> (callUseSet call, valISet vi)
                    INew _ vi _       -> (Map.empty, valISet vi)
                    INewArr _ vi _ v  -> (valSet v, valISet vi)
                    INewStr _ vi _    -> (Map.empty, valISet vi)
                    ICondJmp _ v _ _  -> (valSet v, Set.empty)
                    ILoad _ vi p      -> (ptrValSet p, valISet vi)
                    IStore _ v p      -> (valSet v `Map.union` ptrValSet p, Set.empty)
                    IAddRef _ t vi _     -> (valsSet [vi], Set.empty)
                    IPhi _ vi phis    -> (phiUseSet phis, valISet vi)
                    _                 -> (Map.empty, Set.empty)
              in fmap (\(pos, _) -> (pos, emptyLiveness { liveUse = use, liveKill = kill })) instr


iterateLiveness :: CFG a Liveness -> CFG a Liveness
iterateLiveness = globalLiveness . linearMap localLiveness

globalLiveness :: CFG a Liveness -> CFG a Liveness
globalLiveness cfg_@(CFG g) = linearMap go cfg_
    where
        go node =
            let out = foldr (Map.unionWith min . liveIn . nodeLiveness . (g OrdMap.!)) Map.empty (OrdSet.elems $ node ^. nodeOut)
                (lastInstr, instrs) = splitLast $ node ^. nodeBody
                lastInstr' = fmap (\(p, l) -> (p, l {liveOut = Map.map (first (+1)) out})) lastInstr
            in  node & nodeBody .~ (instrs ++ [lastInstr'])


localLiveness :: Node a Liveness -> Node a Liveness
localLiveness node = node & nodeBody %~ go
    where
        go :: [Instr (a, Liveness)] -> [Instr (a, Liveness)]
        go []             = []
        go (instr:instrs) =
            let xs = go instrs
                instr' = (\(p, live) ->
                    let out = case xs of
                            []  -> liveOut live
                            x:_ -> liveIn $ snd $ single x
                        fromOut = Map.filterWithKey (\k _ -> not $ k `Set.member` liveKill live) $ Map.map (first (+1)) out
                        fromThis = Map.map (0,) (liveUse live)
                        in_ = fromThis `Map.union` fromOut
                    in (p, live {liveOut = out, liveIn = in_}))
                    <$> instr
            in  instr':xs

callUseSet :: Call a -> TypedVarSet
callUseSet call = case call of
    Call _ _ _ vs _    -> valsSet vs
    CallVirt _ _ _ vs -> valsSet vs

phiUseSet :: [PhiVariant a] -> TypedVarSet
phiUseSet phis = valsSet $ map (\(PhiVar _ _ v) -> v) phis


valISet :: IRValueName -> VarSet
valISet (IRValueName s) = Set.singleton s

valSet :: Val a -> TypedVarSet
valSet v = case v of
    VVal _ t (IRValueName s) -> Map.singleton s (() <$ t)
    _                     -> Map.empty

valsSet :: [Val a] -> TypedVarSet
valsSet = foldr (Map.union . valSet) Map.empty

ptrValSet :: Ptr a -> TypedVarSet
ptrValSet ptr = case ptr of
    PArrLen _ v                -> valSet v
    PElem _ _ v1 v2            -> valsSet [v1, v2]
    PFld _ _ v _               -> valSet v
    PLocal {}                  -> Map.empty
    PParam _ t _ (IRValueName vi) -> Map.singleton vi (() <$ t)

nodeLiveness :: Node a Liveness -> Liveness
nodeLiveness node = single $ head $ nodeCode node

instance Show Liveness where
    show l = "in = " ++ showMap (liveIn l) ++
             ", out = " ++ showMap (liveOut l) ++
             ", use = " ++ showMap (liveUse l) ++
             ", kill = " ++ showSet (liveKill l)
        where showSet = show . Set.toList
              showMap :: (Show a, Show b) => Map.HashMap a b -> String
              showMap = show . Map.toList
