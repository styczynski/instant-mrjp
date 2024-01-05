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
import qualified Utils.Containers.IDMap as IM
import qualified Typings.Env as TypeChecker

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
setFunction fn = ((\e -> lcStateSet (\env -> env & functions .~ e))) =<< (IM.insertM (idMapFailure "setFunction" (\i -> Errors.ILNEDuplicateFunction i fn)) fn) =<< (lcStateGet (^. functions))

overrideFunction :: LS.Function Position -> LinearConverter ()
overrideFunction fn = ((\e -> lcStateSet (\env -> env & functions .~ e))) =<< (IM.overrideM (idMapFailure "overrideFunction" Errors.ILNEUndefinedFunction) fn) =<< (lcStateGet (^. functions))

getFunction :: String -> LinearConverter (LS.Function Position)
getFunction n = (IM.findM (idMapFailure "getFunction" Errors.ILNEUndefinedFunction) n) =<< (lcStateGet (^. functions))

failure :: ((TypeChecker.TypeCheckerEnv, LinearTranslatorEnv) -> Errors.Error) -> LinearConverter a
failure errorFactory = throwError . errorFactory . (\env -> (env^.typings, env)) =<< get

idMapFailure :: String -> (String -> Errors.InternalLNError) -> (String -> LinearConverter a)
idMapFailure n fn = (\id -> failure (\(tcEnv, lnEnv) -> Errors.InternalLinearizerFailure tcEnv lnEnv n $ fn id))