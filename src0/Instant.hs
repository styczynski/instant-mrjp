{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Instant where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.List as L
import qualified Data.Text as T
import Instant.Backend.Base
import Instant.Errors.Errors as Errors
import Instant.Typings.Checker as Typings
import Instant.Logs
import Instant.Parser.Parser
import Instant.Syntax
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process

runCLI :: InstantBackend -> IO ()
runCLI backend = evaluateInstantPipeline $ runPipeline backend

runPipeline :: InstantBackend -> InstantPipeline ()
runPipeline backend = do
  args <- liftIO $ getArgs
  case args of
    file : rest -> do
      let noBin = "no-bin" `elem` rest
          jasminName = replaceExtension file (inputExtension backend)
      contents <- liftIO $ readFile file
      printLogInfo $ "Parsing: " <> (T.pack file)
      parsedAST <- parseInstant file contents
      case parsedAST of
        Left _ -> instantError $ "Parser encountered a critical errors (see logs above): " <> (T.pack file)
        Right node -> do
          printLogInfo $ "Input file was correctly parsed: " <> (T.pack file)
          checkerResponse <- Typings.typecheck node
          case checkerResponse of
            Left e -> do
              Errors.printErrors e file contents
              instantError "Typechecker discovered some problems."
            Right _ -> do
              backendResponse <- runBackend jasminName (takeFileName jasminName) node backend
              case backendResponse of
                Just e -> instantError e
                Nothing -> do
                  instantSuccess
    _ -> instantError "BAD ARGS"
