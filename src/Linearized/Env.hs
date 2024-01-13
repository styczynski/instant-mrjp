{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Linearized.Env where

import qualified Data.Map as M
import qualified Utils.Containers.IDMap as IM
import qualified Linearized.Syntax as LS
import Control.Lens
import Control.DeepSeq
import GHC.Generics (Generic)
import qualified Optimizer.Env as Optimizer
import qualified Typings.Env as TypeChecker

import Reporting.Errors.Position

data LinearTranslatorEnv a = LinearTranslatorEnv
  {
    _ltVarNameCounter :: Int
    , _ltVarMap :: M.Map String String
    , _ltVarType :: M.Map String (LS.Type Position)
    , _ltStructures :: IM.Map (LS.Structure Position)
    , _ltFunctions :: IM.Map (LS.Function Position)
    , _ltDatas :: IM.Map (LS.DataDef Position)
    , _ltTypings :: TypeChecker.TypeCheckerEnv
    , _ltOptimizerState :: a
    , _ltReturnContextType :: Maybe (LS.Type Position)
  } deriving (Show, Generic, NFData)


makeLensesWith abbreviatedFields ''LinearTranslatorEnv

createInitialEnv ::  Optimizer.OptimizerEnv () -> (LinearTranslatorEnv ())
createInitialEnv oEnv = LinearTranslatorEnv {
    _ltVarNameCounter = 0
    , _ltVarMap = M.empty
    , _ltVarType = M.empty
    , _ltStructures = IM.empty
    , _ltFunctions = IM.empty
    , _ltDatas = IM.empty
    , _ltTypings = (oEnv^.Optimizer.tCEnv)
    , _ltReturnContextType = Nothing
    , _ltOptimizerState = ()
}

scrapInternalState :: LinearTranslatorEnv a -> LinearTranslatorEnv ()
scrapInternalState oEnv = oEnv { _ltOptimizerState = () }