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
import qualified Data.Text as T

type LinearConverter o a = (StateT (LinearTranslatorEnv o) (ExceptT Errors.Error LattePipeline)) a

oStateGet :: (o -> t) -> LinearConverter o t
oStateGet fn = lcStateGet (fn . (^. optimizerState))

oStateSet :: (o -> o) -> LinearConverter o ()
oStateSet fn = lcStateSet (\env -> env & optimizerState %~ fn)

lcState :: LinearConverter o (LinearTranslatorEnv o)
lcState = get

lcStateGet :: (LinearTranslatorEnv o -> t) -> LinearConverter o t
lcStateGet = (flip (<$>)) lcState

lcStateSet :: (LinearTranslatorEnv o -> LinearTranslatorEnv o) -> LinearConverter o ()
lcStateSet modifyFn = modify modifyFn

withLCState :: (LinearTranslatorEnv o -> LinearTranslatorEnv o) -> LinearConverter o a -> LinearConverter o a
withLCState modifyFn = withStateT modifyFn

liftPipelineOpt :: LattePipeline a -> LinearConverter o a
liftPipelineOpt = lift . lift

setFunction :: LS.Function Position -> LinearConverter o ()
setFunction fn = ((\e -> lcStateSet (\env -> env & functions .~ e))) =<< (IM.insertM (idMapFailure "setFunction" (\i -> Errors.ILNEDuplicateFunction i fn)) fn) =<< (lcStateGet (^. functions))

overrideFunction :: LS.Function Position -> LinearConverter o ()
overrideFunction fn = ((\e -> lcStateSet (\env -> env & functions .~ e))) =<< (IM.overrideM (idMapFailure "overrideFunction" Errors.ILNEUndefinedFunction) fn) =<< (lcStateGet (^. functions))

getFunction :: String -> LinearConverter o (LS.Function Position)
getFunction n = (IM.findM (idMapFailure "getFunction" Errors.ILNEUndefinedFunction) n) =<< (lcStateGet (^. functions))

failure :: ((TypeChecker.TypeCheckerEnv, LinearTranslatorEnv o) -> Errors.Error) -> LinearConverter o a
failure errorFactory = throwError . errorFactory . (\env -> (env^.typings, env)) =<< get

idMapFailure :: String -> (String -> Errors.InternalLNError) -> (String -> LinearConverter o a)
idMapFailure n fn = (\id -> failure (\(tcEnv, lnEnv) -> Errors.InternalLinearizerFailure tcEnv (scrapInternalState lnEnv) n $ fn id))

runInternal :: String -> (x -> LinearConverter o' a) -> o' -> x -> LinearConverter o a
runInternal label fn initialState arg = do
    liftPipelineOpt $ printLogInfo $ T.pack $ "Running linearizer internal transformation: " ++ label
    prevInternalState <- lcStateGet (^. optimizerState)
    inState <- lcStateGet (\env -> env {_ltOptimizerState = initialState})
    r <- liftPipelineOpt $ _runInternal inState label fn initialState arg
    case r of 
        (_, Left err) -> failure $ const err
        (newEnv, Right internalResult) -> do
            lcStateSet (\_ -> newEnv { _ltOptimizerState = prevInternalState })
            liftPipelineOpt $ printLogInfo $ T.pack $ "Successfully completed internal linearizer transformation: " ++ label
            return internalResult
    where
        _runInternal :: (LinearTranslatorEnv o') -> String -> (x -> LinearConverter o' a) -> o' -> x -> LattePipeline (LinearTranslatorEnv o', Either (Errors.Error) a)
        _runInternal inState label fn initialState arg = do
            result <- either (return . Left) (\((result), env) -> return $ Right (env, result)) =<< runExceptT (runStateT (fn arg) inState)
            case result of
                (Left err) -> return (inState, Left err)
                (Right (newEnv, internalResult)) -> return (newEnv, Right internalResult)