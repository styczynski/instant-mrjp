module Optimizer.Def where

import Control.Monad.Except hiding (void)
import Control.Monad.Reader hiding (void)
import Control.Monad.State hiding (void)

import qualified Reporting.Errors.Def as Errors
import Reporting.Logs
import Optimizer.Env

type Optimizer s a = (StateT (OptimizerEnv s) (ExceptT Errors.Error LattePipeline)) a

