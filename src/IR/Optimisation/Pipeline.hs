module IR.Optimisation.Pipeline where

import           IR.Flow.Liveness
import           IR.Flow.SSA
import           IR.Optimisation.CommonSubexpressions
import           IR.Optimisation.DeadCode
import           IR.Optimisation.Propagation
import           IR.Syntax.Syntax
import           IR.Utils

-- Main optimisation pipeline.
optimise :: SSA () -> Method () -> SSA ()
optimise ssa mthd = fixpoint ((`propagateCopiesAndConsts` mthd) .
                    (() <$) .
                    globalCommonSubexpressionElimination .
                    removeDeadCodeSSA .
                    analyseLivenessSSA) ssa

analyseLivenessSSA :: SSA () -> SSA Liveness
analyseLivenessSSA (SSA g) = SSA $ analyseLiveness g
