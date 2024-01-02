module Optimizer.Def where

import Control.Lens

import Control.Monad.Except hiding (void)
import Control.Monad.Reader hiding (void)
import Control.Monad.State hiding (void)

import qualified Typings.Env as TypeChecker
import qualified Reporting.Errors.Def as Errors
import Reporting.Logs
import Optimizer.Env

type Optimizer s a = (StateT (OptimizerEnv s) (ExceptT Errors.Error LattePipeline)) a

tcState :: Optimizer s (TypeChecker.TypeCheckerEnv)
tcState = (\env -> env^.tCEnv) <$> get

oState :: Optimizer s s
oState = (\env -> env^.internalState) <$> get

oStateGet :: (s -> t) -> Optimizer s t
oStateGet = (flip (<$>)) oState

oStateSet :: (s -> s) -> Optimizer s ()
oStateSet modifyFn = modify (\env -> env & internalState %~ modifyFn)

withOState :: (s -> s) -> Optimizer s a -> Optimizer s a
withOState modifyFn = withStateT (\env -> env & internalState %~ modifyFn)

scrapInternalState :: OptimizerEnv a -> OptimizerEnv ()
scrapInternalState env = env { _oeInternalState = () }

failure :: ((TypeChecker.TypeCheckerEnv, OptimizerEnv ()) -> Errors.Error) -> Optimizer s a
failure errorFactory = throwError . errorFactory . (\env -> (env^.tCEnv, scrapInternalState env)) =<< get

liftPipelineOpt :: LattePipeline a -> Optimizer s a
liftPipelineOpt = lift . lift