{-# LANGUAGE ImpredicativeTypes #-}
module Typings.Def where

import qualified Reporting.Errors.Def as Errors
import qualified Program.Syntax as Syntax
import qualified Typings.Types as Type
import Reporting.Errors.Position
import Reporting.Logs

import Control.Monad.Except hiding (void)
import Control.Monad.Reader hiding (void)
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE

--type Environment = ([Class], [Function], [(Ident Position, Type Position)])

--type VarEnv   = Map AST.VarId AST.Type
--type FunEnv   = Map AST.FunId (AST.Type, [AST.Type])
--type ConEnv   = Map (Maybe AST.ConstructorId) (AST.ClassMemberAccess, [AST.Type])


type FunctEnv = M.Map String Type.Function
type ClassEnv = M.Map String Type.Class
type VarEnv = M.Map Type.Name Type.Type

data TypeCheckerEnv = TypeCheckerEnv
  {
  _teDefinedFuns      :: FunctEnv
  , _teCurrentScopeName :: Maybe String
  , _teCurrentClass     :: Maybe String
  , _teClassEnv         :: ClassEnv
  , _teLoc              :: Maybe Position
  , _teRetType          :: Maybe Type.Type
  , _teCurrentScopeVars :: VarEnv
  }

initialEnv :: TypeCheckerEnv
initialEnv = TypeCheckerEnv
  { _teDefinedFuns      = M.fromList []
  , _teCurrentScopeName = Nothing
  , _teCurrentClass     = Nothing
  , _teClassEnv         = M.empty
  , _teLoc              = Nothing
  , _teRetType          = Nothing
  , _teCurrentScopeVars = M.empty
  }


type TypeChecker a = (ReaderT TypeCheckerEnv (ExceptT Errors.Error LattePipeline)) a 

type TypeCheckingResult = Either Errors.Error (Syntax.Program Position, [Type.Class])

failure :: Errors.Error -> TypeChecker ()
failure err = throwError err