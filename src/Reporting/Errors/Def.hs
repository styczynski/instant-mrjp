{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Reporting.Errors.Def where
import qualified Program.Syntax as Syntax
import qualified Typings.Types as Type
import qualified Data.List.NonEmpty as NEL
import Prelude hiding ((<>))

import Typings.Env(TypeCheckerEnv)

data Error
  =
   UnknownFailure String
   | NoMain TypeCheckerEnv
   | InvalidMainReturn Type.Function TypeCheckerEnv
   | MainHasArgs Type.Function TypeCheckerEnv
  -- = MainType
  -- | NoMain
  -- | TypeMatch Type Type
  -- | OperatorTypeMatch AnyOp [(Type, Type)] (Type, Type)
  -- | ArgNumFun FunId Int Int
  -- | ArgNumMethod ClassId MethodId Int Int
  -- | ArgNumConstructor ClassId (Maybe ConstructorId) Int Int
  -- | DuplicateVar VarId
  | UnknownParent Type.Class String
  | CyclicInheritanceSelf Type.Class 
  | CyclicInheritance Type.Class [Type.Class] String
  | DuplicateFun Type.Function [Type.Function]
  | DuplicateClass Type.Class [Type.Class]
  | DuplicateMember Type.Class Type.Member [Type.Member]
  -- | DuplicateMethod ClassId MethodId
  -- | DuplicateConstructor ClassId (Maybe ConstructorId)
  -- | ClassMatch ClassId ClassId
  -- | UndefinedVar VarId
  -- | UndefinedFun FunId
  -- | UndefinedClass ClassId
  -- | UndefinedField ClassId FieldId
  -- | UndefinedMethod ClassId MethodId
  -- | UndefinedConstructor ClassId (Maybe ConstructorId)
  -- | NoReturn
  -- | NotAClass Type
  -- | ThisNotInClass
  -- | SuperNotInClass
  -- | ThisArgName
  -- | BadPrivateAccess ClassId String
  -- | BadProtectedAccess ClassId String
  -- | NoSuperClass ClassId
  -- | BadSuperUse


