{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module Optimizer.Env where

import qualified Typings.Env as TypeChecker

import Control.Lens

data OptimizerEnv s = OptimizerEnv
  {
  _oeInternalState :: s
  , _oeTCEnv :: TypeChecker.TypeCheckerEnv
  } deriving (Show)

makeLensesWith abbreviatedFields ''OptimizerEnv

createInitialState :: TypeChecker.TypeCheckerEnv -> s -> OptimizerEnv s
createInitialState tcEnv internalState = OptimizerEnv {
    _oeInternalState = internalState
    , _oeTCEnv = tcEnv
}