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
import qualified Instant.JVM as JVM
import qualified Instant.LLVM as LLVM

import Instant.Syntax
import Instant.Logs
import qualified Data.Text as T
import qualified Data.List as L

parse :: String -> String -> Either String ICode
parse filename code = fromAST <$> parseInstant filename code

runCLI :: IO ()
runCLI = evaluateInstantPipeline runPipeline

runPipeline :: InstantPipeline ()
runPipeline = do
  args <- liftIO $ getArgs
  case args of
    file:rest -> do
      let noBin = "no-bin" `elem` rest
          jasminName = replaceExtension file "j"
      contents <- liftIO $ readFile file
      printLogInfo $ "Parsing: " <> (T.pack file)
      ast <- return $ parse file contents
      printLogInfo $ "Parsed: " <> (T.pack file)
      case ast >>= JVM.build (takeFileName jasminName) of
        Left e -> instantError $ "Parsing problem: " <> (T.pack file) <> ": " <> (T.pack e)
        Right jasminCode -> do
          printLogInfo $ "Writing Jasmine code to " <> (T.pack jasminName)
          liftIO $ writeFile jasminName jasminCode
          when (not noBin) $ do
            let outpath = takeDirectory file
            callJasmine jasminName outpath
    _ -> instantError "BAD ARGS"

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

callJasmine :: FilePath -> FilePath -> InstantPipeline ()
callJasmine jasminName outpath = execCmd "java" [ "-jar", "/home/students/inf/PUBLIC/MRJP/Jasmin/jasmin.jar", jasminName, "-d", outpath]
    
