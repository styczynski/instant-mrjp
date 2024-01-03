module Linearized.Linearizer where

import Control.Monad.Except(runExceptT)
import Control.Monad.State

import qualified Program.Syntax as A
import qualified Linearized.Syntax as B

import Reporting.Errors.Position
import qualified Reporting.Errors.Def as Errors
import Reporting.Logs
import Linearized.Syntax (IRPosition(..))
import Linearized.Env
import Linearized.Def
import qualified Linearized.Converter as Converter
import qualified Optimizer.Env as Optimizer

type LinearizerResult = Either Errors.Error (LinearTranslatorEnv, B.Program IRPosition)

posFrom :: Position -> IRPosition
posFrom pos = IRPosition 0 (pos, pos)

runLinearizer :: (A.Program Position) -> LinearConverter (B.Program IRPosition)
runLinearizer prog = do
    rawIR <- Converter.transform prog
    ir <- return $ fmap (posFrom) rawIR
    return ir

linearizeToIR ::  Optimizer.OptimizerEnv () -> A.Program Position -> LattePipeline LinearizerResult
linearizeToIR oEnv prog@(A.Program pos defs) = do
    either (return . Left) (\((ir), env) -> return $ Right (env, ir)) =<< runExceptT (runStateT (runLinearizer prog) $ createInitialEnv oEnv)
