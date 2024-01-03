{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module Linearized.Env where

import qualified Data.Map as M
import qualified Linearized.Syntax as LS
import Control.Lens
import qualified Optimizer.Env as Optimizer
import qualified Typings.Env as TypeChecker

import Reporting.Errors.Position

data LinearTranslatorEnv = LinearTranslatorEnv
  {
    _ltVarNameCounter :: Int
    , _ltVarMap :: M.Map String String
    , _ltVarType :: M.Map String (LS.Type Position)
    , _ltStructures :: M.Map String (LS.Structure Position)
    , _ltFunctions :: M.Map String (LS.Function Position)
    , _ltStrings :: M.Map String (LS.Label Position)
    , _ltTypings :: TypeChecker.TypeCheckerEnv
  } deriving (Show)

makeLensesWith abbreviatedFields ''LinearTranslatorEnv

createInitialEnv ::  Optimizer.OptimizerEnv () -> LinearTranslatorEnv
createInitialEnv oEnv = LinearTranslatorEnv {
    _ltVarNameCounter = 0
    , _ltVarMap = M.empty
    , _ltVarType = M.empty
    , _ltStructures = M.empty
    , _ltFunctions = M.empty
    , _ltStrings = M.empty
    , _ltTypings = (oEnv^.Optimizer.tCEnv)
}