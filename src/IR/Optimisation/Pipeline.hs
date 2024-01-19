module IR.Optimisation.Pipeline where

import           IR.Flow.Liveness
import           IR.Flow.SSA
import           IR.Optimisation.CommonSubexpressions
import           IR.Optimisation.DeadCode
import           IR.Optimisation.Propagation
import           IR.Syntax.Syntax
import           IR.Utils

-- Main optimisation pipeline.
optimise :: SSA a () -> Method a -> SSA a ()
optimise ssa mthd = fixpoint ((`propagateCopiesAndConsts` mthd) .
                    (() <$) .
                    globalCommonSubexpressionElimination .
                    removeDeadCodeSSA .
                    analyseLivenessSSA) ssa

analyseLivenessSSA :: SSA a () -> SSA a Liveness
analyseLivenessSSA (SSA g) = SSA $ analyseLiveness g
