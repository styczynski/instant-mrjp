{-# LANGUAGE DeriveTraversable #-}
module IR.Compl where

import Data.Functor
import           Data.Bifunctor                              (Bifunctor (first))
import           IR.Flow.CFG
import           IR.Flow.Liveness               (Liveness,
                                                              analyseLiveness,
                                                              emptyLiveness)
import           IR.Flow.SSA
import           IR.Optimisation.CFGTransform          (inlineTrivialBlocks,
                                                              removeUnreachable)
import           IR.Optimisation.DeadCode
import           IR.Optimisation.Pipeline
import IR.RegisterAllocation.InterferenceGraph
import IR.RegisterAllocation.RegisterAllocation
import IR.Utils
import IR.Phi
--import IR.CodeGen.Generator
import IR.Syntax.Syntax

data CompiledProg a =
    CompiledProg a (Metadata a) [(CFG a Liveness, Method a, RegisterAllocation)]
    deriving (Show)

instance Eq (CompiledProg a) where
    (==) (CompiledProg _ meta1 methods1) (CompiledProg _ meta2 methods2) =
        let normalizeMethod = (\(cfg, method, reg) -> (() <$ cfg, () <$ method)) in
        let methods1' = map normalizeMethod methods1 in
        let methods2' = map normalizeMethod methods2 in
        (() <$ meta1) == (() <$ meta2) && methods1' == methods2'

instance Functor (CompiledProg) where
    fmap f (CompiledProg pos meta methods) = CompiledProg (f pos) (fmap f meta) (map (\(cfg, method, reg) -> (mapCFGPos f cfg, fmap f method, reg)) methods)



compl_ :: (Program a) -> CompiledProg a
compl_ (Program pos meta mthds) = 
    let cfgs = zip (map cfg mthds) mthds
        cfgsLin = map (uncurry removeUnreachable) cfgs
        cfgsWithLiveness = map (first analyseLiveness) cfgsLin
        ssaCode = map (\(g, mthd) -> (transformToSSA g mthd, mthd)) cfgsWithLiveness
        optimisedCode = map (\(ssa, mthd) -> (optimise ssa mthd, mthd)) ssaCode
        optimisedWithLiveness = map (first (\(SSA g) -> SSA $ analyseLiveness g)) optimisedCode
        allocatedCfgs = map (\(cfg_, mthd) ->
            let (ig_, cfg') = getColouredInterferenceGraph cfg_
            in  (cfg', mthd, ig_)) optimisedWithLiveness
        unfoldedPhi = map (\(SSA c, m, g) -> (unfoldPhi (SSA c) m (getRegisterAllocation c g), m, g)) allocatedCfgs
        optimisedCfgs = map (\(c, m, g) -> let(c', m') = removeUnreachable (inlineTrivialBlocks c) m in (c', m', g)) unfoldedPhi
        finalCfgs = map (\(c, m, g) -> (removeDeadCode $ analyseLiveness c, m, g)) optimisedCfgs
        --assembly = generate meta (map (\(c, m, g) -> (c, m, getRegisterAllocation c g)) finalCfgs) in
    --assembly
    in
    --show $ (map (\(c, m, _) -> (c, m)) finalCfgs)
    CompiledProg pos meta (map (\(c, m, g) -> (c, m, getRegisterAllocation c g)) finalCfgs)
