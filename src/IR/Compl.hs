module IR.Compl where

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
import IR.CodeGen.Generator
import IR.Syntax.Syntax

compl_ :: (Program ()) -> String
compl_ (Program _ meta mthds) = 
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
    generate meta (map (\(c, m, g) -> (c, m, getRegisterAllocation c g)) finalCfgs)
