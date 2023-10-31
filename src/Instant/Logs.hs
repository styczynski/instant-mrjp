{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Instant.Logs where

import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Exit

import Colog (
    LogAction,
    Message,
    Msg (..),
    WithLog,
    cmap,
    fmtMessage,
    formatWith,
    logError,
    logInfo,
    logWarning,
    logTextStderr,
    logTextStdout,
    msgSeverity,
    msgText,
    richMessageAction,
    showSeverity,
    usingLoggerT,
 )

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

type InstantPipeline t = forall env m . (WithLog env Message m, MonadIO m) => m t

printLogInfo :: Text.Text -> InstantPipeline ()
printLogInfo = logInfo

printLogWarn :: Text.Text -> InstantPipeline ()
printLogWarn = logWarning

instantError :: Text.Text -> InstantPipeline ()
instantError err = do
    logError err
    liftIO $ exitFailure

logStdoutAction :: LogAction IO Message
logStdoutAction = cmap fmtMessage logTextStdout

evaluateInstantPipeline :: InstantPipeline t -> IO t
evaluateInstantPipeline pipeline = do
    k <- usingLoggerT logStdoutAction pipeline
    return $ k