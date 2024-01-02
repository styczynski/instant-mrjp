{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module Optimizer.Env where

import qualified Typings.Env as TypeChecker
import Reporting.Errors.Position

import Control.Lens

data OptimizerEnv s = OptimizerEnv
  {
  _oeInternalState :: s
  , _oePositionTrace :: [Position]
  , _oeTCEnv :: TypeChecker.TypeCheckerEnv
  } deriving (Show)

makeLensesWith abbreviatedFields ''OptimizerEnv

optimizerEnter :: Position -> OptimizerEnv s -> OptimizerEnv s
optimizerEnter pos env = env & positionTrace %~ (:) pos

optimizerQuit :: Position -> OptimizerEnv s -> OptimizerEnv s
optimizerQuit pos env = env & positionTrace %~ tail

nextInternalState :: s -> OptimizerEnv t -> OptimizerEnv s
nextInternalState st env = env { _oeInternalState = st }

createInitialState :: TypeChecker.TypeCheckerEnv -> s -> OptimizerEnv s
createInitialState tcEnv internalState = OptimizerEnv {
    _oeInternalState = internalState
    , _oePositionTrace = []
    , _oeTCEnv = tcEnv
}