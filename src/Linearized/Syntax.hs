{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Linearized.Syntax where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Control.Lens hiding(Empty, Index, Const)
import Data.Generics.Product
import Data.Generics.Sum
import GHC.Generics (Generic)
import Reporting.Errors.Position

data IRPosition = IRPosition Int (Position, Position)

data Program a = Program a [Structure a] [Function a] [(Label a, String)]
    deriving (Eq, Ord, Read, Generic, Foldable, Traversable, Functor)

data Structure a = Struct a (Label a) (Maybe (Label a)) Size [(Label a, Type a, Offset)] [{-method-}Label a]
    deriving (Eq, Ord, Read, Generic, Foldable, Traversable, Functor)

data Type a = IntT a | ByteT a | Reference a
    deriving (Eq, Ord, Read, Generic, Foldable, Traversable, Functor)

data Function a = Fun a (Label a) (Type a) [{-args-}(Type a, Name a)] [Stmt a]
    deriving (Eq, Ord, Read, Generic, Foldable, Traversable, Functor)

data Label a = Label a String
    deriving (Eq, Ord, Read, Generic, Foldable, Traversable, Functor)

data Name a = Name a String
    deriving (Eq, Ord, Read, Generic, Foldable, Traversable, Functor)

type Size = Integer
type Index = Integer
type Offset = Integer

data Stmt a = VarDecl a (Type a) (Name a) (Expr a)
          | Assign a (Type a) (Target a) (Expr a)
          | IncrCounter a (Name a)
          | DecrCounter a (Name a)
          | ReturnVal a (Type a) (Expr a)
          | Return a
          | SetLabel a (Label a)
          | Jump a (Label a)
          | JumpCmp a (Cmp a) (Label a) (Value a) (Value a)
          deriving (Eq, Ord, Read, Generic, Foldable, Traversable, Functor)

data Cmp a = Eq a | Ne a | Le a | Lt a | Ge a | Gt a
    deriving (Eq, Ord, Read, Generic, Foldable, Traversable, Functor)

data Target a = Variable a (Name a) | Array a (Name a) (Value a) | Member a (Name a) Offset
    deriving (Eq, Ord, Read, Generic, Foldable, Traversable, Functor)

data Expr a = NewObj a (Label a)
          | NewArray a (Type a) (Value a)
          | NewString a (Label a)
          | Val a (Value a)
          | Call a (Label a) [Value a] --function call
          | MCall a (Name a) Index [Value a] --method call
          | ArrAccess a (Name a) (Value a)
          | MemberAccess a (Name a) Offset
          | IntToByte a (Value a)
          | ByteToInt a (Value a)
          | Not a (Value a)
          | BinOp a (Op a) (Value a) (Value a)
          | Cast a (Label a) (Value a)
    deriving (Eq, Ord, Read, Generic, Foldable, Traversable, Functor)

data Value a = Const a (Constant a) | Var a (Name a)
    deriving (Eq, Ord, Read, Generic, Foldable, Traversable, Functor)

data Op a = Add a | Sub a | Mul a | Div a | Mod a | And a | Or a
    deriving (Eq, Ord, Read, Generic, Foldable, Traversable, Functor)

data Constant a = IntC a Integer | ByteC a Integer | StringC a (Label a)| Null a
    deriving (Eq, Ord, Read, Generic, Foldable, Traversable, Functor)

instance Show (Constant a) where
    show (IntC _ i) = "<int>"++show i
    show (ByteC _ b) = "<byte>"++show b
    show (StringC _ str) = "<str>"++show str
    show (Null _) = "<null>"

instance Show (Label a) where
    show (Label _ l) = l

instance Show (Name a) where
    show (Name _ n) = n

instance Show (Program a) where
    show (Program _ ss fs strs) = intercalate "\n" (map show ss) ++"\n"++ intercalate "\n" (map show fs) ++ "\n" ++ (concat $ map (\(l,s) -> show l++": "++show s++"\n") strs)

instance Show (Structure a) where
    show (Struct _ l _ _ fs ms) = "struct "++show l++"\n"++(concat $ map (\(l,t,_)-> "    "++show t++" "++show l++";\n") fs)++(concat $ map (\l->"    "++show l++"(...)\n") ms)

instance Show (Function a) where
    show (Fun _ l t args body) = show t++" "++show l++"("++intercalate ", " (map (\(t,n)->show t++" "++show n) args)++")\n"++(concat $ map (\s->show s++"\n") body)

instance Show (Stmt a) where
    show (VarDecl _ t n e) = "    "++show t ++ " "++show n++" = "++show e
    show (Assign _ t g e) = "    "++show g ++" = "++show e
    show (IncrCounter _ n) = "    inc "++show n
    show (DecrCounter _ n) = "    decr "++show n
    show (ReturnVal _ t e) = "    return "++show e
    show (Return _) = "    return"
    show (SetLabel _ l) = "  "++show l++":"
    show (Jump _ l) = "    jump "++show l
    show (JumpCmp _ cmp l vl vr) = "    jump "++show l++" if "++show vl++" "++show cmp++" "++show vr

instance Show (Expr a) where
    show (Val _ v) = show v
    show (MemberAccess _ n o) = show n++".field["++show o++"]"
    show (ArrAccess _ n v) = show n++"["++show v++"]"
    show (BinOp _ op v1 v2) = show v1 ++" "++ show op ++" "++ show v2
    show (NewObj _ l) = "new "++show l
    show (NewArray _ t v) = "new "++show t++"["++show v++"]"
    show e = show e

instance Show (Target a) where
    show (Variable _ v) = show v
    show (Array _ a v) = show a++"["++show v++"]"
    show( Member _ m o) = show m++".field["++show o++"]"

instance Show (Type a) where
    show (IntT _) = "int"
    show (ByteT _) = "byte"
    show (Reference _) = "obj"

instance Show (Value a) where
    show (Const _ c) = show c
    show (Var _ x) = show x

instance Show (Op a) where
    show (Add _) = "+"
    show (Sub _) = "-"
    show (Mul _) = "*"
    show (Div _) = "/"
    show (Mod _) = "%"
    show (And _) = "&&"
    show (Or _) = "||"

instance Show (Cmp a) where
    show (Eq _) = "=="
    show (Ne _) = "!="
    show (Lt _) = "<"
    show (Gt _) = ">"
    show (Le _) = "<="
    show (Ge _) = ">="
