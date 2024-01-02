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

data Program a = Program a [Structure a] [Function a] [(Label a, String)]
    deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor)

data Structure a = Struct a (Label a) (Maybe (Label a)) Size [(Label a, Type a, Offset)] [{-method-}Label a]
    deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor)

data Type a = IntT a | ByteT a | Reference a
    deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor)

data Function a = Fun a (Label a) (Type a) [{-args-}(Type a, Name a)] [Stmt a]
    deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor)

data Label a = Label a String
    deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor)

data Name a = Name a String
    deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor)

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
          deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor)

data Cmp a = Eq a | Ne a | Le a | Lt a | Ge a | Gt a
    deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor)

data Target a = Variable a (Name a) | Array a (Name a) (Value a) | Member a (Name a) Offset
    deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor)

data Expr a = NewObj (Label a)
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
    deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor)

data Value a = Const a (Constant a) | Var a (Name a)
    deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor)

data Op a = Add a | Sub a | Mul a | Div a | Mod a | And a | Or a
    deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor)

data Constant a = IntC a Integer | ByteC a Integer | StringC a (Label a)| Null a
    deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor)

linShow (Program ss fs strs) = intercalate "\n" (map linShowStruct ss) ++"\n"++ intercalate "\n" (map linShowFun fs) ++ "\n" ++ (concat $ map (\(l,s) -> l++": "++show s++"\n") strs)

linShowStruct (Struct l _ _ fs ms) = "struct "++l++"\n"++(concat $ map (\(l,t,_)-> "    "++show t++" "++l++";\n") fs)++(concat $ map (\l->"    "++l++"(...)\n") ms)

linShowFun (Fun l t args body) = show t++" "++l++"("++intercalate ", " (map (\(t,n)->show t++" "++n) args)++")\n"++(concat $ map (\s->linShowStmt s++"\n") body)

linShowStmt (VarDecl t n e) = "    "++show t ++ " "++n++" = "++linShowExp e
linShowStmt (Assign t g e) = "    "++linShowTarget g ++" = "++linShowExp e
linShowStmt (IncrCounter n) = "    inc "++n
linShowStmt (DecrCounter n) = "    decr "++n
linShowStmt (ReturnVal t e) = "    return "++linShowExp e
linShowStmt (Return) = "    return"
linShowStmt (SetLabel l) = "  "++l++":"
linShowStmt (Jump l) = "    jump "++l
linShowStmt (JumpCmp cmp l vl vr) = "    jump "++show l++" if "++show vl++" "++show cmp++" "++show vr

linShowExp (Val v) = show v
linShowExp (MemberAccess n o) = n++".field["++show o++"]"
linShowExp (ArrAccess n v) = n++"["++show v++"]"
linShowExp (BinOp op v1 v2) = show v1 ++" "++ show op ++" "++ show v2
linShowExp (NewObj l) = "new "++l
linShowExp (NewArray t v) = "new "++show t++"["++show v++"]"
linShowExp e = show e

linShowTarget (Variable v) = v
linShowTarget (Array a v) = a++"["++show v++"]"
linShowTarget (Member m o) = m++".field["++show o++"]"

instance Show (Type a) where
    show IntT = "int"
    show ByteT = "byte"
    show Reference = "obj"

instance Show (Value a) where
    show (Const c) = show c
    show (Var x) = x

instance Show (Op a) where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Mod = "%"
    show And = "&&"
    show Or = "||"

instance Show (Cmp a) where
    show (Eq _) = "=="
    show (Ne _) = "!="
    show (Lt _) = "<"
    show (Gt _) = ">"
    show (Le _) = "<="
    show (Ge _) = ">="
