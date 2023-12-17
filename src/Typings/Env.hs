{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TemplateHaskell #-}  
{-# LANGUAGE FlexibleInstances #-} 
module Typings.Env where

import Control.Lens

import qualified Program.Syntax as Syntax
import qualified Typings.Types as Type
import qualified Data.Text as T
import qualified Data.FuzzySet.Simple as Fuzz
import Reporting.Errors.Position

import Control.Monad.Except hiding (void)
import Control.Monad.Reader hiding (void)
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE

import Control.Monad.Trans.Class


type FunctEnv = M.Map String Type.Function

type ClassEnv = M.Map String Type.Class
type VarEnv = M.Map Type.Name Type.Type

data TypeCheckerEnv = TypeCheckerEnv
  {
  _teDefinedFuns      :: FunctEnv
  , _teDefinedClasses   :: ClassEnv
  , _teDefinedFunsFuzz :: Fuzz.FuzzySet
  , _teCurrentScopeName :: Maybe String
  , _teCurrentClass     :: Maybe String
  , _teClassEnv         :: ClassEnv
  , _teLoc              :: Maybe Position
  , _teRetType          :: Maybe Type.Type
  , _teCurrentScopeVars :: VarEnv
  }

makeLensesWith abbreviatedFields ''TypeCheckerEnv

initialEnv :: TypeCheckerEnv
initialEnv = TypeCheckerEnv
  { _teDefinedFuns      = M.empty
  , _teDefinedClasses   = M.empty
  ,  _teDefinedFunsFuzz = Fuzz.defaultSet
  , _teCurrentScopeName = Nothing
  , _teCurrentClass     = Nothing
  , _teClassEnv         = M.empty
  , _teLoc              = Nothing
  , _teRetType          = Nothing
  , _teCurrentScopeVars = M.empty
  }


findFunction :: TypeCheckerEnv -> String -> Maybe Type.Function
findFunction env = (flip M.lookup) (env^.definedFuns)

setupDefEnv :: FunctEnv -> ClassEnv -> TypeCheckerEnv -> TypeCheckerEnv 
setupDefEnv fns cls env =
    env {
        _teDefinedFuns = fns
        , _teDefinedFunsFuzz = Fuzz.fromList $ map T.pack $ M.keys fns
        , _teDefinedClasses = cls
    }