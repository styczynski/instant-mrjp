{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module Linearized.Env where

import qualified Data.Map as M
import qualified Linearized.Syntax as LS
import Control.Lens

data LinearTranslatorEnv = LinearTranslatorEnv
  {
    _ltVarNameCounter :: Int
    , _ltVarMap :: M.Map String String
    , _ltVarType :: M.Map String (LS.Type LS.IRPosition)
    , _ltStructures :: M.Map String (LS.Structure LS.IRPosition)
    , _ltFunctions :: M.Map String (LS.Function LS.IRPosition)
    , _ltStrings :: M.Map String (LS.Label LS.IRPosition)
  } deriving (Show)

makeLensesWith abbreviatedFields ''LinearTranslatorEnv

initialEnv :: LinearTranslatorEnv
initialEnv = LinearTranslatorEnv {
    _ltVarNameCounter = 0
    , _ltVarMap = M.empty
    , _ltVarType = M.empty
    , _ltStructures = M.empty
    , _ltFunctions = M.empty
    , _ltStrings = M.empty
}