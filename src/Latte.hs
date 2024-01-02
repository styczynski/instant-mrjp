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
import Reporting.Errors.Errors

import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process

runCLI :: () -> IO ()
runCLI backend = evaluateLattePipeline $ runPipeline backend

runPipeline :: () -> LattePipeline ()
runPipeline backend = do
  args <- liftIO $ getArgs
  case args of
    file : rest -> do
      --let noBin = "no-bin" `elem` rest
      --    jasminName = replaceExtension file (inputExtension backend)
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
            Right (tcEnv, prog, _) -> do
              printLogInfo $ "Typecheck done" <> (T.pack file)
              optimizerResult <- Optimizer.optimize tcEnv prog
              case optimizerResult of
                Left err -> do
                  printLogInfo $ "Optimizer failed"
                  printErrors err file contents parsedAST
                Right (prog) -> do
                  printLogInfo $ "Optimization done" <> (T.pack file)
    _ -> latteError "BAD ARGS"
