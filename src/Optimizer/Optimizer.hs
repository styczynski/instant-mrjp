module Optimizer.Optimizer(optimize) where

import Control.Monad.Except(runExceptT)
import Control.Monad.State

import qualified Typings.Env as TypeChecker
import qualified Reporting.Errors.Def as Errors
import qualified Program.Syntax as Syntax
import Optimizer.Env
import qualified Optimizer.ConstPropagation as ConstPropagation
import Reporting.Logs
import Reporting.Errors.Position

optimize :: TypeChecker.TypeCheckerEnv -> (Syntax.Program Position) -> LattePipeline (Either Errors.Error (Syntax.Program Position))
optimize tcEnv prog = do
    result <- return $ runExceptT (evalStateT (ConstPropagation.run prog) (createInitialState tcEnv ConstPropagation.initialState))
    result