module Linearized.Def where

import Control.Lens

import Control.Monad.Except hiding (void)
import Control.Monad.Reader hiding (void)
import Control.Monad.State hiding (void)

import qualified Reporting.Errors.Def as Errors
import Reporting.Logs
import Linearized.Env

import Reporting.Errors.Position
import qualified Linearized.Syntax as LS
import qualified Data.Map as M

type LinearConverter a = (StateT LinearTranslatorEnv (ExceptT Errors.Error LattePipeline)) a

lcState :: LinearConverter LinearTranslatorEnv
lcState = get

lcStateGet :: (LinearTranslatorEnv -> t) -> LinearConverter t
lcStateGet = (flip (<$>)) lcState

lcStateSet :: (LinearTranslatorEnv -> LinearTranslatorEnv) -> LinearConverter ()
lcStateSet modifyFn = modify modifyFn

withLCState :: (LinearTranslatorEnv -> LinearTranslatorEnv) -> LinearConverter a -> LinearConverter a
withLCState modifyFn = withStateT modifyFn

liftPipelineOpt :: LattePipeline a -> LinearConverter a
liftPipelineOpt = lift . lift

setFunction :: LS.Function Position -> LinearConverter ()
setFunction fn@(LS.Fun _ (LS.Label _ id) _ _ _) = lcStateSet (\env -> env & functions %~ M.insert id fn)

