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
        (Left err) -> do
            printErrors err file contents
            latteError "Failed to parse input files"
        (Right ast) -> do
          printLogInfo $ "Parsed: " <> (T.pack file) <> (T.pack $ show ast)
    _ -> latteError "BAD ARGS"
