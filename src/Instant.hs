{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Instant where

import Control.Monad
import Control.Exception
import System.Environment
import System.Exit
import System.IO
import System.FilePath
import System.Process

import Control.Monad.IO.Class (MonadIO, liftIO)

import           Instant.Parse
import Instant.Backend
import qualified Instant.JVM as JVM
import qualified Instant.LLVM as LLVM

import Instant.Syntax
import Instant.Logs
import qualified Data.Text as T
import qualified Data.List as L


runCLI :: IO ()
runCLI = evaluateInstantPipeline runPipeline


runPipeline :: InstantPipeline ()
runPipeline = do
  args <- liftIO $ getArgs
  backend <- return $ JVM.backend
  case args of
    file:rest -> do
      let noBin = "no-bin" `elem` rest
          jasminName = replaceExtension file (inputExtension backend)
      contents <- liftIO $ readFile file
      printLogInfo $ "Parsing: " <> (T.pack file)
      parsedAST <- parseInstant file contents
      case parsedAST of
        Left _ -> instantError $ "Parser encountered a critical errors (see logs above): " <> (T.pack file)
        Right node -> do
            printLogInfo $ "Input file was correctly parsed: " <> (T.pack file)
            backendResponse <- runBackend jasminName (takeFileName jasminName) node backend
            case backendResponse of
                Just e -> instantError e
                Nothing -> do
                    printLogInfo $ "OK"
    _ -> instantError "BAD ARGS"

    
