module IR.Optimisation.DeadCode (removeDeadCode, removeDeadCodeSSA) where

import Control.Lens hiding (Const)

import qualified Data.HashMap.Strict           as Map
import           IR.Flow.CFG
import           IR.Flow.Liveness
import           IR.Flow.SSA
import           IR.Syntax.Syntax
import           IR.Utils

-- Remove assignments to dead variables and unreachable code.
removeDeadCode :: CFG a Liveness -> CFG a Liveness
removeDeadCode = linearMap (\n -> n & nodeBody %~ loopRemove True)

removeDeadCodeSSA :: SSA a Liveness -> SSA a Liveness
removeDeadCodeSSA (SSA g) = SSA $ removeDeadCode g

loopRemove :: Bool -> [Instr (a, Liveness)] -> [Instr (a, Liveness)]
loopRemove _ [] = []
loopRemove reachable (instr:instrs) =
    let live = liveOut $ snd $ single instr
    in case instr of
            ILabel {}         -> instr:cont True
            ILabelAnn {}      -> instr:cont True
            _ | not reachable -> cont False
            IVRet _           -> instr:cont False
            IRet _ _          -> instr:cont False
            IJmp {}           -> instr:cont False
            ICondJmp {}       -> instr:cont False
            IVCall {}         -> instr:cont True
            ICall {}          -> instr:cont True
            IStore {}         -> instr:cont True
            IEndPhi {}        -> instr:cont True
            IOp _ vi _ _ _
                | toStr vi `Map.member` live -> instr:cont True
            ISet _ vi _
                | toStr vi `Map.member` live -> instr:cont True
            IUnOp _ vi _ _
                | toStr vi `Map.member` live -> instr:cont True
            INew _ vi _
                | toStr vi `Map.member` live -> instr:cont True
            INewArr _ vi _ _
                | toStr vi `Map.member` live -> instr:cont True
            INewStr _ vi _
                | toStr vi `Map.member` live -> instr:cont True
            ILoad _ vi _
                | toStr vi `Map.member` live -> instr:cont True
            IPhi _ vi _
                | toStr vi `Map.member` live -> instr:cont True
            ISwap _ _ vi _
                | toStr vi `Map.member` live -> instr:cont True
            _                 -> cont reachable
    where
        cont b = loopRemove b instrs
