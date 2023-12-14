{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Instant.Logs where

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
  )
import System.IO(hPutStrLn, stderr)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Exit

type InstantPipeline t = forall env m. (WithLog env Message m, MonadIO m) => m t

printLogInfo :: Text.Text -> InstantPipeline ()
printLogInfo = logInfo

printLogWarn :: Text.Text -> InstantPipeline ()
printLogWarn = logWarning

instantError :: Text.Text -> InstantPipeline ()
instantError err = do
  liftIO $ hPutStrLn stderr "ERROR\n"
  logError err
  liftIO $ exitFailure

logStdoutAction :: LogAction IO Message
logStdoutAction = cmap fmtMessage logTextStdout

instantSuccess :: InstantPipeline ()
instantSuccess = do
  liftIO $ hPutStrLn stderr "OK\n"

evaluateInstantPipeline :: InstantPipeline t -> IO t
evaluateInstantPipeline pipeline = do
  k <- usingLoggerT logStdoutAction pipeline
  return $ k
