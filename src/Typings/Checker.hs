{-# LANGUAGE ImpredicativeTypes #-}
module Typings.Checker where

import Typings.Def
import qualified Typings.Types as Type
import qualified Reporting.Errors.Def as Errors
import qualified Typings.TopLevelCollector as Collector
import Program.Syntax
import Control.Monad.Except(runExceptT)
import Control.Monad.Reader(runReaderT)
import Reporting.Errors.Position
import Reporting.Logs

performTypeCheck :: Program Position -> TypeChecker (Program Position, [Type.Class])
performTypeCheck prog@(Program pos defs) = do
    defs <- Collector.collectDefinitions defs
    return (prog, [])

checkTypes :: Program Position -> LattePipeline TypeCheckingResult
checkTypes prog@(Program pos defs) = do
    result <- return $ runExceptT (runReaderT (performTypeCheck prog) initialEnv)
    result

    -- let classes = addBuiltInTypes $ map changeEmptyParent classDefs
    -- checkCyclesInClasses classes
    -- checkRedeclarationInClasses classes
    -- funDefs <- getFunctions defs
    -- let functions = addBuiltInFunctions funDefs
    -- checkRedeclarationInFunctions functions
    -- checkForMain functions
    -- np <- runReaderT (checkP prog) (classes, functions, [])
