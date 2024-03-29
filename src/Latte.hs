{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Latte where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.List as L
import qualified Data.Text as T

import Reporting.Logs
import Parser.Parser
import Parser.Types
import Parser.Transform
import Typings.Checker as TypeChecker
import Optimizer.Optimizer as Optimizer
import Linearized.Linearizer as Linearizer
import Backend.Base as Backend
import Backend.X64.X64 as BackendX64
import Reporting.Errors.Errors
import qualified Linearized.Syntax as LSyntax

import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process

import Utils.Similarity

import IR.Syntax.Print(printTree)
import qualified IR.Compl as Compl

usedBackend = BackendX64.backend

runCLI :: () -> IO ()
runCLI backend = evaluateLattePipeline LogEverything $ runPipeline backend

runPipeline :: () -> LattePipeline ()
runPipeline backend = do
  args <- liftIO $ getArgs
  case args of
    file : rest -> do
      contents <- liftIO $ readFile file
      printLogInfo $ "Parsing: " <> (T.pack file)
      parsedAST <- parseLatte file contents
      case parsedAST of 
        p@(ProgramParseError err) -> do
            printErrors err file contents p
            latteError "Failed to parse input files"
        ast -> do
          printLogInfo $ "Parsed: " <> (T.pack file)
          prog <- transformAST file ast
          printLogInfo $ "Transformed: " <> (T.pack file) 
          typingResult <- TypeChecker.checkTypes prog
          case typingResult of
            Left err -> do
              printLogInfo $ "Typecheck failed"
              printErrors err file contents parsedAST
              latteError "Typecheck failed"
            Right (tcEnv, prog', _) -> do
              printLogInfo $ "Typecheck done" <> (T.pack file) <> "\n\n" <> (T.pack $ printi 0 prog')
              optimizerResult <- Optimizer.optimize tcEnv prog'
              case optimizerResult of
                Left err -> do
                  printLogInfo $ "Optimizer failed"
                  printErrors err file contents parsedAST
                  latteError "Optimizer failed"
                Right (optimizerEnv, prog'') -> do
                  printLogInfo $ "Optimization done" <> (T.pack file) <> "\n\n" <> (T.pack $ printi 0 prog'')
                  irResult <- Linearizer.linearizeToIR optimizerEnv prog''
                  case irResult of
                    Left err -> do
                      printLogInfo $ "IR conversion failed"
                      printErrors err file contents parsedAST
                      latteError "IR conversion failed"
                    Right (_, ir) -> do
                      printLogInfo $ "IR conversion done" <> (T.pack file) <> "\n\n" <> (T.pack $ printTree ir)
                      compiledProg <- ((Compl.compl_ ir) :: (LattePipeline (Compl.CompiledProg LSyntax.IRPosition))) --(fmap (const ()) ir)
                      printLogInfo $ "COMPL_ DONE" <> (T.pack file) <> "\n\n" <> (T.pack $ show compiledProg)
                      let outputPath = replaceExtension file (inputExtension usedBackend)
                      backendResult <- Backend.runBackend outputPath (takeFileName outputPath) compiledProg usedBackend
                      case backendResult of 
                        (Left err) -> do
                          printLogInfo $ T.pack $ "Backend code generation has failed"
                          printErrors err file contents parsedAST
                          latteError "Backend code generation has failed"
                        (Right (_, _, outputFilePath)) -> do
                          printLogInfo $ "Backend code generation completed successfully " <> (T.pack file) <> " -> " <> (T.pack outputFilePath)
                          printLogInfo $ "DONE"
                          latteSuccess
    _ -> latteError "BAD ARGS"
