module Optimizer.Optimizer(optimize) where

import Control.Monad.Except(runExceptT)
import Control.Monad.State

import qualified Typings.Env as TypeChecker
import qualified Reporting.Errors.Def as Errors
import qualified Program.Syntax as Syntax
import Optimizer.Env
import qualified Optimizer.ConstPropagation as ConstPropagation
import qualified Optimizer.ReturnChecker as ReturnChecker
import Reporting.Logs
import Reporting.Errors.Position

optimize :: TypeChecker.TypeCheckerEnv -> (Syntax.Program Position) -> LattePipeline (Either Errors.Error (OptimizerEnv (), Syntax.Program Position))
optimize tcEnv prog = do
    optResult <- runExceptT (runStateT (ConstPropagation.run prog) (createInitialState tcEnv ConstPropagation.initialState))
    --return $ Right prog
    case optResult of 
        (Left err) -> return $ Left $ err
        (Right (optProg, optState)) -> do
            checkedProg <- runExceptT (runStateT (ReturnChecker.run optProg) (nextInternalState ReturnChecker.initialState optState))
            return $ either (Left) (\(p, env) -> Right (scrapInternalState env, p)) checkedProg