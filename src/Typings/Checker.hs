{-# LANGUAGE ImpredicativeTypes #-}
module Typings.Checker where

import Typings.Def
import Typings.Env
import qualified Typings.Types as Type
import qualified Reporting.Errors.Def as Errors
import qualified Typings.TopLevelCollector as Collector
import Program.Syntax
import Control.Monad.Except(runExceptT)
import Control.Monad.State
import Reporting.Errors.Position
import Reporting.Logs

import Utils.Similarity

checkForMain :: TypeChecker ()
checkForMain = do
    env <- get
    case findFunction env "main" of
        Nothing -> failure $ Errors.NoMain env
        (Just fn@(Type.Fun _ retType _ _)) | (not $ similar retType (IntT Undefined)) -> failure $ Errors.InvalidMainReturn fn env
        (Just fn@(Type.Fun _ _ args _)) | length args > 0 -> failure $ Errors.MainHasArgs fn env
        (Just _) -> return ()

performTypeCheck :: Program Position -> TypeChecker (Program Position, [Type.Class])
performTypeCheck prog@(Program pos defs) = do
    Collector.collectDefinitions defs
    checkForMain
    return (prog, [])

checkTypes :: Program Position -> LattePipeline TypeCheckingResult
checkTypes prog@(Program pos defs) = do
    result <- return $ runExceptT (evalStateT (performTypeCheck prog) initialEnv)
    result

    -- let classes = addBuiltInTypes $ map changeEmptyParent classDefs
    -- checkCyclesInClasses classes
    -- checkRedeclarationInClasses classes
    -- funDefs <- getFunctions defs
    -- let functions = addBuiltInFunctions funDefs
    -- checkRedeclarationInFunctions functions
    -- checkForMain functions
    -- np <- runReaderT (checkP prog) (classes, functions, [])
