{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Reporting.Logs where

import Control.Lens

import Colog
  ( LogAction,
    Message,
    Msg (..),
    WithLog,
    cmap,
    fmtMessage,
    formatWith,
    logError,
    logInfo,
    logTextStderr,
    logTextStdout,
    logWarning,
    msgSeverity,
    msgText,
    richMessageAction,
    showSeverity,
    usingLoggerT,
    liftLogAction,
    LoggerT,
  )
import Control.Monad.Trans.Class(MonadTrans)
import System.IO(hPutStrLn, stderr)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Exit

import Control.Monad.State hiding (void)

data LogLevel = LogNothing | LogEverything | LogDebug | LogInfo | LogWarn | LogError deriving (Show, Eq)

instance Ord LogLevel where
    compare a b = compare (relativeRank a) (relativeRank b) where
         relativeRank LogNothing = 5
         relativeRank LogError = 4
         relativeRank LogWarn = 3
         relativeRank LogInfo = 2
         relativeRank LogDebug = 1
         relativeRank LogEverything = 0

data LoggerState = LoggerState {
  _logstLogLevel :: LogLevel
} deriving (Show, Eq)

makeLensesWith abbreviatedFields ''LoggerState

--type LattePipeline t = forall env m. (WithLog env Message m, MonadIO m) => m t
type LattePipeline = StateT (LoggerState) (LoggerT Message IO)
--type WithLattePipeline env m = (WithLog env Message m, MonadIO m)

--type LattePipelineT = LoggerT Message IO

liftPipeline :: (Monad m, MonadTrans t) => LogAction m msg -> LogAction (t m) msg
liftPipeline = liftLogAction

printLogInfo :: Text.Text -> LattePipeline ()
printLogInfo = doForLogLevel LogInfo $ lift . logInfo

doForLogLevel :: LogLevel -> (t -> LattePipeline ()) -> (t -> LattePipeline ())
doForLogLevel requiredLL action arg = do
  currentLL <- gets (^.logLevel)
  when (currentLL <= requiredLL) (action arg)

printLogWarn :: Text.Text -> LattePipeline ()
printLogWarn = doForLogLevel LogWarn $ lift . logWarning

latteError :: Text.Text -> LattePipeline ()
latteError err = do
  lift $ liftIO $ hPutStrLn stderr "ERROR\n"
  lift $ logError err
  lift $ liftIO $ exitFailure

logStdoutAction :: LogAction IO Message
logStdoutAction = cmap fmtMessage logTextStdout

latteSuccess :: LattePipeline ()
latteSuccess = do
  lift $ liftIO $ hPutStrLn stderr "OK\n"

evaluateLattePipeline :: LogLevel -> LattePipeline t -> IO t
evaluateLattePipeline logLevel pipeline = do
  let initialState = (LoggerState { _logstLogLevel = logLevel })
  k <- usingLoggerT logStdoutAction (evalStateT pipeline initialState)
  return $ k

printLogInfoStr :: String -> LattePipeline ()
printLogInfoStr = printLogInfo . Text.pack 