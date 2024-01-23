{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Latte.Compiler as Compiler
import qualified Backend.X64.X64 as BackendX64
import System.Environment
import Control.Monad.IO.Class

import Control.Lens
import Reporting.Logs

main :: IO ()
main = do
    args <- liftIO $ getArgs
    case args of
        file : rest -> do
            let config = (Compiler.defaultConfigFor BackendX64.backend) & Compiler.loggingLevel .~ LogEverything
            Compiler.compileLatteThen config (Compiler.inputFile file) resultHandler
    where 
        resultHandler :: Compiler.CompilationResult -> LattePipeline ()
        resultHandler result = do
            case result of
                (Compiler.CompilationFailed err file contents rawProgram) -> do
                    Compiler.printCompilerErrors err file contents rawProgram
                    latteError "Optimizer failed" 
                (Compiler.CompilationOK output) -> do
                    printLogInfo $ "DONE"
                    latteSuccess
