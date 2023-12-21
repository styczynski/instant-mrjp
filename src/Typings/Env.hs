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
import Data.Tuple.Append

import Control.Monad.Except hiding (void)
import Control.Monad.Reader hiding (void)
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE
import qualified Utils.Graphs as G

import Control.Monad.Trans.Class

data Inheritance = ClassExtends String String deriving (Show)
data Hierarchy = Hierarchy (G.Graph String Type.Class Inheritance) (M.Map String [Type.Class])

type FunctEnv = M.Map String Type.Function

type ClassEnv = M.Map String Type.Class
type VarEnv = M.Map String (Type.Name, Type.Type)

data TypeCheckerEnv = TypeCheckerEnv
  {
  _teDefinedFuns      :: FunctEnv
  , _teDefinedClasses   :: Hierarchy
  , _teDefinedFunsFuzz :: Fuzz.FuzzySet
  , _teCurrentScopeName :: Maybe String
  , _teCurrentClass     :: Maybe Type.Class
  , _teCurrentFunction  :: Maybe Type.Function
  , _teClassEnv         :: ClassEnv
  , _teLoc              :: Maybe Position
  , _teRetType          :: Maybe Type.Type
  , _teCurrentScopeVars :: VarEnv
  , _teParentScopes     :: [(Position, VarEnv)]
  }

makeLensesWith abbreviatedFields ''TypeCheckerEnv

initialEnv :: TypeCheckerEnv
initialEnv = TypeCheckerEnv
  { _teDefinedFuns      = M.empty
  , _teDefinedClasses   = Hierarchy G.empty M.empty
  ,  _teDefinedFunsFuzz = Fuzz.defaultSet
  , _teCurrentScopeName = Nothing
  , _teCurrentClass     = Nothing
  , _teCurrentFunction  = Nothing
  , _teClassEnv         = M.empty
  , _teLoc              = Nothing
  , _teRetType          = Nothing
  , _teCurrentScopeVars = M.empty
  , _teParentScopes     = []
  }


findFunction :: TypeCheckerEnv -> String -> Maybe Type.Function
findFunction env = (flip M.lookup) (env^.definedFuns)

setupDefEnv :: FunctEnv -> ClassEnv -> Hierarchy -> TypeCheckerEnv -> TypeCheckerEnv 
setupDefEnv fns cls classHierarchy env =
    env {
        _teDefinedFuns = fns
        , _teDefinedFunsFuzz = Fuzz.fromList $ map T.pack $ M.keys fns
        , _teDefinedClasses = classHierarchy
    }

--addVar :: Type.Name -> Type.Type -> TypeCheckerEnv -> TypeCheckerEnv
--addVar name t env = env & currentScopeVars %~ M.insert (Type.stringName name) (name, t)

addVar :: Type.Name -> Type.Type -> TypeCheckerEnv -> Either (Type.Name, Type.Name, Type.Type) TypeCheckerEnv
addVar name t env = (\(old, newEnv) -> maybe (Right newEnv) (Left . (<++) name) $ M.lookup (Type.stringName name) old) $ env & currentScopeVars <<%~ M.insert (Type.stringName name) (name, t)
--addVar name t env = (\(old, newEnv) -> maybe (Right newEnv) (\(prevName, prevType) -> Left (name, prevName, prevType)) $ M.lookup (Type.stringName name) old) $ env & currentScopeVars <<%~ M.insert (Type.stringName name) (name, t)


separateScope :: Position -> TypeCheckerEnv -> TypeCheckerEnv
separateScope pos env = env
  & parentScopes %~ (:) (pos, env^.currentScopeVars)
  & currentScopeVars .~ M.empty