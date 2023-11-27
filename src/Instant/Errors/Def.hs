{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Instant.Errors.Def where

import Instant.Syntax
import qualified Data.List.NonEmpty as NEL
import qualified Instant.Syntax as AST

import Prelude hiding ((<>))


data Error
  = MainType
  | NoMain
  | TypeMatch Type Type
  | OperatorTypeMatch AnyOp [(Type, Type)] (Type, Type)
  | ArgNumFun FunId Int Int
  | ArgNumMethod ClassId MethodId Int Int
  | ArgNumConstructor ClassId (Maybe ConstructorId) Int Int
  | DuplicateVar VarId
  | DuplicateFun FunId
  | DuplicateClass ClassId
  | DuplicateField ClassId FieldId
  | DuplicateMethod ClassId MethodId
  | DuplicateConstructor ClassId (Maybe ConstructorId)
  | ClassMatch ClassId ClassId
  | UndefinedVar VarId
  | UndefinedFun FunId
  | UndefinedClass ClassId
  | UndefinedField ClassId FieldId
  | UndefinedMethod ClassId MethodId
  | UndefinedConstructor ClassId (Maybe ConstructorId)
  | NoReturn
  | NotAClass Type
  | ThisNotInClass
  | SuperNotInClass
  | ThisArgName
  | BadPrivateAccess ClassId String
  | BadProtectedAccess ClassId String
  | NoSuperClass ClassId
  | BadSuperUse


