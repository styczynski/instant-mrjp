{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}

module Reporting.Logs where

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

--type LattePipeline t = forall env m. (WithLog env Message m, MonadIO m) => m t
type LattePipeline = LoggerT Message IO

--type WithLattePipeline env m = (WithLog env Message m, MonadIO m)

--type LattePipelineT = LoggerT Message IO

liftPipeline :: (Monad m, MonadTrans t) => LogAction m msg -> LogAction (t m) msg
liftPipeline = liftLogAction

printLogInfo :: Text.Text -> LattePipeline ()
printLogInfo = logInfo

printLogWarn :: Text.Text -> LattePipeline ()
printLogWarn = logWarning

latteError :: Text.Text -> LattePipeline ()
latteError err = do
  liftIO $ hPutStrLn stderr "ERROR\n"
  logError err
  liftIO $ exitFailure

logStdoutAction :: LogAction IO Message
logStdoutAction = cmap fmtMessage logTextStdout

latteSuccess :: LattePipeline ()
latteSuccess = do
  liftIO $ hPutStrLn stderr "OK\n"

evaluateLattePipeline :: LattePipeline t -> IO t
evaluateLattePipeline pipeline = do
  k <- usingLoggerT logStdoutAction pipeline
  return $ k
