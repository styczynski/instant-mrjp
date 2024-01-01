{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Reporting.Errors.Def where
import qualified Program.Syntax as Syntax
import qualified Typings.Types as Type
import qualified Data.List.NonEmpty as NEL
import Prelude hiding ((<>))
import Data.Typeable
import Reporting.Errors.Position
import Typings.Env(TypeCheckerEnv)

data TypeContext =
  TypeInFunctionReturn (Syntax.Definition Position)
  | TypeInClassParent (Syntax.Definition Position)
  | TypeInClassField (Syntax.ClassDecl Position)
  | TypeInMethodReturn (Syntax.ClassDecl Position)
  | TypeInVarDecl (Syntax.DeclItem Position)
  | TypeInFunctionArgDecl (Syntax.Definition Position) (Syntax.Arg Position)
  | TypeInMethodArgDecl (Syntax.ClassDecl Position) (Syntax.Arg Position)
  | TypeInCast (Syntax.Expr Position)
  | TypeInNew (Syntax.Expr Position)
  deriving (Show, Typeable)

data InternalTCError =
  ITCEClassContextNotAvailable (Maybe String)
  | ITCEFunctionContextNotAvailable (Maybe String)
  | ITCEMissingClassMember String String
  | ITCEDuplicateMethodArg Type.Name Type.Name Type.Type
  | ITCEDuplicateFunctionArg Type.Name Type.Name Type.Type
  | ITCEMissingMember String String
  deriving (Show, Typeable)


data Error
  =
   UnknownFailure TypeCheckerEnv String
   | NumericConstantExceedsTypeLimit TypeCheckerEnv (Syntax.Lit Position) Integer [(String, Syntax.Type Position)]
   | InternalTypecheckerFailure TypeCheckerEnv String InternalTCError
   | UnknownVariable TypeCheckerEnv Type.Name
   | UnknownType TypeCheckerEnv Type.Name 
   | CallIncompatibleNumberOfParameters TypeCheckerEnv (Syntax.Expr Position) (Syntax.Type Position) [(Syntax.Expr Position, Type.Type)]
   | CallNotCallableType TypeCheckerEnv Type.Type [(Syntax.Expr Position, Type.Type)]
   | NewUsageOnString TypeCheckerEnv (Syntax.Expr Position)
   | NewUsageOnNonClass TypeCheckerEnv Type.Type (Syntax.Expr Position)
   | NewArrayNonNumericDimensions TypeCheckerEnv Type.Type (Syntax.Expr Position) (Syntax.Expr Position)
   | ArrayAccessNonNumericIndex TypeCheckerEnv Type.Type (Syntax.Expr Position) (Syntax.Expr Position)
   | IndexAccessNonCompatibleType TypeCheckerEnv Type.Type (Syntax.Expr Position) (Syntax.Expr Position)
   | FieldAccessNonCompatibleType TypeCheckerEnv Type.Type (Syntax.Expr Position) (Syntax.Expr Position)
   | UnknownClassMember TypeCheckerEnv Type.Class (Syntax.Ident Position) [Type.Class]
   | IllegalTypeUsed TypeCheckerEnv TypeContext Type.Type
   | ImpossibleInference TypeCheckerEnv TypeContext Type.Type Position
   | MissingReturnValue TypeCheckerEnv Position Type.Type Type.Function
   | VariableRedeclared TypeCheckerEnv Position (Type.Name, Type.Type) (Type.Name, Type.Type)
   | IncompatibleTypesReturn TypeCheckerEnv (Syntax.Stmt Position) Type.Function Type.Type Type.Type
   | IncompatibleTypesAssign TypeCheckerEnv (Syntax.Stmt Position) Type.Type Type.Type
   | IncompatibleTypesInit TypeCheckerEnv (Syntax.DeclItem Position) Type.Type Type.Type
   | DuplicateFunctionArgument TypeCheckerEnv Type.Function (Syntax.Arg Position) [(Syntax.Arg Position)]
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
  | InheritanceLinearizationProblem Type.Class String
  | UnknownParent Type.Class String
  | CyclicInheritanceSelf Type.Class
  | CyclicInheritance Type.Class [Type.Class] String
  | DuplicateFun Type.Function [Type.Function]
  | DuplicateClass Type.Class [Type.Class]
  | DuplicateMember Type.Class Type.Member [Type.Member]
  | DuplicateMembersInChain Type.Class Type.Member [(Type.Class, Type.Member)]
  deriving (Typeable, Show)
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


