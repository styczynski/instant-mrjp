{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Instant.Typings.Def where

import Instant.Errors.Errors
import qualified Instant.Syntax as AST

import Data.Functor
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map(Map)
import qualified Data.List.NonEmpty as NE
import Control.Monad.Except
import Control.Monad.Reader
import Control.Lens

import Prelude hiding (LT, GT, EQ, (<>))


type Typechecker =
  ExceptT ErrorPack (Reader TypecheckerEnv)
type TopTypechecker =
  Except ErrorPack


type VarEnv   = Map AST.VarId AST.Type
type FunEnv   = Map AST.FunId (AST.Type, [AST.Type])
type ConEnv   = Map (Maybe AST.ConstructorId) (AST.ClassMemberAccess, [AST.Type])
type ClassEnv = Map AST.ClassId ClassEntry


data ClassEntry = ClassEntry
  { _ceSuper        :: Maybe AST.ClassId
  , _ceFields       :: Map AST.FieldId (AST.ClassMemberAccess, AST.Type)
  , _ceMethods      :: Map AST.MethodId (AST.ClassMemberAccess, (AST.Type, [AST.Type]))
  , _ceConstructors :: ConEnv
  }


data TypecheckerEnv = TypecheckerEnv
  { _teDefinedVars      :: VarEnv
  , _teDefinedFuns      :: FunEnv
  , _teCurrentScopeName :: Maybe String
  , _teCurrentClass     :: Maybe AST.ClassId
  , _teClassEnv         :: ClassEnv
  , _teLoc              :: Maybe AST.ASTMeta
  , _teRetType          :: Maybe AST.Type
  , _teCurrentScopeVars :: S.Set AST.VarId
  }


initialEnv :: TypecheckerEnv
initialEnv = TypecheckerEnv
  { _teDefinedVars      = M.singleton "void" AST.TVoid
  , _teDefinedFuns      = M.fromList $
    [ ("printInt", (AST.TVoid, [AST.TInt]))
    , ("printString", (AST.TVoid, [AST.TString]))
    , ("readInt", (AST.TInt, []))
    , ("readString", (AST.TString, []))
    , ("error", (AST.TVoid, []))
    ]
  , _teCurrentScopeName = Nothing
  , _teCurrentClass     = Nothing
  , _teClassEnv         = M.empty
  , _teLoc              = Nothing
  , _teRetType          = Nothing
  , _teCurrentScopeVars = S.empty
  }


makeLensesWith abbreviatedFields ''TypecheckerEnv
makeLensesWith abbreviatedFields ''ClassEntry