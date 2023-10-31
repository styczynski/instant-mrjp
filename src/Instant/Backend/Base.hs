{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Instant.Backend.Base where

import Control.Exception
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.List as L
import qualified Data.Text as T
import Instant.Logs
import Instant.Syntax
import System.Process

type InstantBackendFn = String -> ICode -> InstantPipeline (Either String String)

class SerializableInstruction a where
  toCode :: a -> String


data InstantBackend = InstantBackend
  { name :: String,
    inputExtension :: String,
    run :: InstantBackendFn,
    compileExecutable :: String -> InstantPipeline ()
  }

runBackend :: String -> String -> (ASTNode 'InstantProgram) -> InstantBackend -> InstantPipeline (Maybe T.Text)
runBackend filePath fileName ast backend = do
  normalizedAST <- do
    printLogInfo $ "Normalizing AST"
    return $ fromAST ast
  printLogInfo $ "Running correct compiler backend: " <> (T.pack $ name backend)
  backendResponse <- (run backend) fileName normalizedAST
  case backendResponse of
    Left e -> return $ Just $ "Backend reported an error: " <> (T.pack filePath) <> ": " <> (T.pack e)
    Right outputCode -> do
      printLogInfo $ "Created file: " <> (T.pack filePath)
      liftIO $ writeFile filePath outputCode
      printLogInfo $ "Calling backend compile step: " <> (T.pack $ name backend)
      (compileExecutable backend) filePath
      return Nothing

execCmd :: String -> [String] -> InstantPipeline ()
execCmd cmd args = do
  printLogInfo $ "Executing command '" <> (T.pack $ L.intercalate " " $ [cmd] ++ args) <> "'"
  result <- liftIO safeExec
  case result of
    Left ex -> instantError "Command has failed"
    Right _ -> printLogInfo $ "Command executed successfully."
  return ()
  where
    safeExec :: IO (Either SomeException ())
    safeExec = try $ do
      callProcess cmd args
      return $ ()
