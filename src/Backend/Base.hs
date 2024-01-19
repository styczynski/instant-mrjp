{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend.Base where

import Control.Exception
import Control.Monad.Except hiding (void)
import Control.Monad.Reader hiding (void)
import Control.Monad.State hiding (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.List as L
import qualified Data.Text as T
import Reporting.Logs
import System.Process
import System.IO

import qualified Reporting.Errors.Def as Errors
import qualified IR.Compl as IR

type BackendPipeline = (ExceptT Errors.Error LattePipeline)

liftPipelineToBackend :: LattePipeline a -> BackendPipeline a
liftPipelineToBackend = lift

type LatteBackendFn a = String -> (IR.CompiledProg a) -> BackendPipeline String

class SerializableInstruction a where
  toCode :: a -> String

data LatteBackend a = LatteBackend
  { backendName :: String,
    inputExtension :: String,
    run :: LatteBackendFn a,
    compileExecutable :: String -> BackendPipeline ()
  }

runBackend :: String -> String -> (IR.CompiledProg a) -> LatteBackend a -> LattePipeline (Either Errors.Error (String, String))
runBackend filePath fileName ast backend = do
  p <- runExceptT (runBackend' filePath fileName ast backend)
  return p
  where
    runBackend' :: String -> String -> (IR.CompiledProg a) -> LatteBackend a -> BackendPipeline (String, String)
    runBackend' filePath fileName ast backend = do
      lift $ printLogInfo $ "Running correct compiler backend: " <> (T.pack $ backendName backend)
      outputCode <- (run backend) fileName ast
      -- case backendResponse of
      --   Left e -> return $ Left e
      --   Right outputCode -> do
      lift $ printLogInfo $ "Created file: " <> (T.pack filePath)
      liftIO $ writeFile filePath outputCode
      lift $ printLogInfo $ "Calling backend compile step: " <> (T.pack $ backendName backend)
      (compileExecutable backend) filePath
      return (filePath, outputCode)

execCmd :: String -> [String] -> BackendPipeline ()
execCmd cmd args = do
  lift $ printLogInfo $ "Executing command '" <> (T.pack $ L.intercalate " " $ [cmd] ++ args) <> "'"
  result <- liftIO safeExec
  case result of
    Left ex -> throwError $ Errors.BackendCodeGenerationError (Errors.CGExternalCommandFailed cmd args)
    Right _ -> lift $ printLogInfo $ "Command executed successfully."
  return ()
  where
    safeExec :: IO (Either SomeException ())
    safeExec = try $ do
      (_,_,_,phandle) <- createProcess_ "Backend.execCmd" (proc cmd args){ std_err = UseHandle stdout}
      waitForProcess phandle
      return ()
      --callProcess cmd args
      --return $ ()
