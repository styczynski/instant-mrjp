{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
module Linearized.Syntax where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Control.Lens hiding(Empty, Index, Const)
import Data.Generics.Product
import Data.Generics.Sum
import GHC.Generics (Generic, Generic1)
import Reporting.Errors.Position
import Control.DeepSeq

import qualified Utils.Containers.IDMap as M

class (HasPosition 1 (s Position) (s Position) Position Position
    , HasPosition 1 (s IRPosition) (s IRPosition) IRPosition IRPosition
    --, HasPosition 1 (s Position) (s IRPosition) Position IRPosition
    , Show (s Position)
    , Show (s IRPosition)
    ) => IsIR (s :: * -> *)

data IRPosition = 
    IRPosition Int (Position, Position)
    deriving (Eq, Ord, Generic, NFData)
    
noPosIR :: IRPosition
noPosIR = IRPosition (-1) (Undefined, Undefined)

instance Show (IRPosition) where
    show (IRPosition v _) | v == -1 = ""
    show (IRPosition _ (Undefined, _)) = ""
    show (IRPosition _ (_, Undefined)) = ""
    show (IRPosition _ (p, _)) = show p

data Program a = Program a (M.Map (Structure a)) (M.Map (Function a)) (M.Map (DataDef a))
    deriving (Ord, Read, Generic, Foldable, Traversable, Functor, Generic1, NFData)
instance IsIR Program

--(Type a) [{-args-}(Type a, Name a)]
data Method a = Method a (Label a) (Label a) (Type a) [(Type a, Name a)]
    deriving (Ord, Read, Generic, Foldable, Traversable, Functor, NFData)
instance IsIR Method

data Field a = Field a (Type a) (Label a)
    deriving (Ord, Read, Generic, Foldable, Traversable, Functor, NFData)
instance IsIR Field

data Structure a = Struct a (Label a) [Label a] (M.Map (Method a)) (M.Map (Field a))
    deriving (Ord, Read, Generic, Foldable, Traversable, Functor, NFData)
instance IsIR Structure

data Type a =
    IntT a
    | ByteT a
    | Reference a (Label a)
    | ArrT a (Type a)
    deriving (Ord, Read, Generic, Foldable, Traversable, Functor, Generic1, NFData, NFData1)
instance IsIR Type

data Function a = Fun a (Maybe (Label a)) (Label a) (Type a) [{-args-}(Type a, Name a)] [Stmt a]
    deriving (Ord, Read, Generic, Foldable, Traversable, Functor,  NFData)
instance IsIR Function

data Entity a = FunEntity a String (Function a) | StructEntity a String (Structure a)
    deriving (Ord, Read, Generic, Foldable, Traversable, Functor, Generic1, NFData)
instance IsIR Entity

class IsEntity t where
    toEntity :: t a -> Entity a
instance IsEntity Function where
    toEntity fn@(Fun p _ (Label _ name) _ _ _) = FunEntity p name fn
instance IsEntity Structure where
    toEntity struct@(Struct p (Label _ name) _ _ _) = StructEntity p name struct

data Label a = Label a String
    deriving (Ord, Read, Generic, Foldable, Traversable, Functor, Generic1, NFData, NFData1)
instance IsIR Label

instance M.Idable (Method a) where
    getID (Method _ clsName methodName _ _) = M.getID clsName ++ "." ++ M.getID methodName

instance M.Idable (Field a) where
    getID (Field _ _ fieldName) = M.getID fieldName


instance M.Idable (Label a) where
    getID (Label _ m) = m

instance M.Idable (Name a) where
    getID (Name _ m) = m

instance M.Idable (Function a) where
    getID (Fun _ Nothing l _ _ _) = M.getID l
    getID (Fun _ (Just cls) l _ _ _) = M.getID cls ++ "." ++ M.getID l

instance M.Idable (Structure a) where
    getID (Struct _ l _ _ _) = M.getID l

instance M.Idable (Label a, b, c) where
    getID (l, _, _) = M.getID l

instance M.Idable (Label a, b) where
    getID (l, _) = M.getID l

instance M.Idable (Name a, b) where
    getID (n, _) = M.getID n

instance M.Idable (DataDef a) where
    getID (DataString _ content _) = content

data Name a = Name a String
    deriving (Ord, Read, Generic, Foldable, Traversable, Functor, Generic1, NFData, NFData1)
instance IsIR Name

type Size = Integer
type Index = Integer
type Offset = Integer

data Stmt a = VarDecl a (Type a) (Name a) (Expr a)
          | Assign a (Type a) (Target a) (Expr a)
          | VCall a (Type a) (Label a) [Value a] --function call
          | VMCall a (Type a) (Name a) (Label a) (Label a) [Value a] --method call
          | IncrCounter a (Name a)
          | DecrCounter a (Name a)
          | ReturnVal a (Type a) (Expr a)
          | Return a
          | SetLabel a (Label a)
          | Jump a (Label a)
          | JumpCmp a (Cmp a) (Label a) (Value a) (Value a)
          deriving (Ord, Read, Generic, Foldable, Traversable, Functor, Generic1, NFData, NFData1)
instance IsIR Stmt

data DataDef a = DataString a (String) (Label a)
    deriving (Ord, Read, Generic, Foldable, Traversable, Functor, Generic1, NFData, NFData1)
instance IsIR DataDef

data Cmp a = Eq a | Ne a | Le a | Lt a | Ge a | Gt a
    deriving (Ord, Read, Generic, Foldable, Traversable, Functor, Generic1, NFData, NFData1)
instance IsIR Cmp

data Target a = Variable a (Name a) | Array a (Name a) (Value a) | Member a (Name a) (Label a) (Label a)
    deriving (Ord, Read, Generic, Foldable, Traversable, Functor, Generic1, NFData, NFData1)
instance IsIR Target

data Expr a = NewObj a (Label a)
          | NewArray a (Type a) (Value a)
          | NewString a (Label a)
          | Val a (Value a)
          | Call a (Label a) [Value a] --function call
          | MCall a (Name a) (Label a) (Label a) [Value a] --method call
          | ArrAccess a (Name a) (Value a)
          | MemberAccess a (Name a) (Label a) (Label a) (Type a)
          | IntToByte a (Value a)
          | ByteToInt a (Value a)
          | Not a (Value a)
          | BinOp a (Op a) (Value a) (Value a)
          | Cast a (Label a) (Value a)
    deriving (Ord, Read, Generic, Foldable, Traversable, Functor, Generic1, NFData, NFData1)
instance IsIR Expr

data Value a = Const a (Constant a) | Var a (Name a) (Type a)
    deriving (Ord, Read, Generic, Foldable, Traversable, Functor, Generic1, NFData, NFData1)
instance IsIR Value

data Op a = Add a | Sub a | Mul a | Div a | Mod a | And a | Or a
    deriving (Ord, Read, Generic, Foldable, Traversable, Functor, Generic1, NFData, NFData1)
instance IsIR Op

data Constant a =
    IntC a Integer
    | ByteC a Integer
    | StringC a (Label a)
    | Null a (Type a)
    deriving (Ord, Read, Generic, Foldable, Traversable, Functor, Generic1, NFData, NFData1)
instance IsIR Constant

instance Show (Entity a) where
    show (FunEntity _ name fn) = "entity<fn, name:"++show name++">"++show fn
    show (StructEntity _ name s) = "entity<struct, name:"++show name++">"++show s

instance Show (Constant a) where
    show (IntC _ i) = "<int>"++show i
    show (ByteC _ b) = "<byte>"++show b
    show (StringC _ str) = "<str>"++show str
    show (Null _ t) = "<"++show t++">null"

instance Show (Label a) where
    show (Label _ l) = l

instance Show (Name a) where
    show (Name _ n) = n

instance Show (Program a) where
    show (Program _ ss fs datas) = intercalate "\n" (M.mapList (curry $ show . snd) ss) ++"\n"++ intercalate "\n" (M.mapList (curry $ show . snd) fs) ++ "\n" ++ intercalate "\n" (M.mapList (curry $ show . snd) datas)

instance Show (Method a) where
    show (Method _ cls name retType args) = "method "++show retType++" "++show cls++"."++show name++" ("++(intercalate ", " $ map (\(aName, aType) -> show aType++" "++show aName) args)++")"

instance Show (Field a) where
    show (Field _ fieldType name) = "field "++show fieldType++" "++show name++";"

instance Show (Structure a) where
    show (Struct _ l chain methods fields) = "struct "++show l++" extends ["++(intercalate ", " $ map show chain)++"]\n"++(concat $ M.mapList (\_ field-> "    "++show field++"\n") fields)++(concat $ M.mapList (\_ method->"    "++show method++"\n") methods)

instance Show (Function a) where
    show (Fun _ Nothing l t args body) = show t++" "++show l++"("++intercalate ", " (map (\(t,n)->show t++" "++show n) args)++")\n"++(concat $ map (\s->show s++"\n") body)
    show (Fun _ (Just cls) l t args body) = show t++" "++show cls++"."++show l++"("++intercalate ", " (map (\(t,n)->show t++" "++show n) args)++")\n"++(concat $ map (\s->show s++"\n") body)

instance Show (DataDef a) where
    show (DataString _ content label) = "data string " ++ show label ++ " = " ++ show content

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
    show (VCall _ t l args) = "    discard<"++show t++"> call<function> " ++ show l ++ "(" ++ intercalate ", " (map show args) ++ ")"
    show (VMCall _ t n cls i args) = "    discard<"++show t++"> call<method:" ++ show i ++ ", class:" ++ show cls ++ "> " ++ show n ++ "(" ++ intercalate ", " (map show args) ++ ")"

instance Show (Expr a) where
    show (Val _ v) = show v
    show (MemberAccess _ n cls field fieldType) = show n++".field<"++show fieldType++">["++show cls++"."++show field++"]"
    show (ArrAccess _ n v) = show n++"["++show v++"]"
    show (BinOp _ op v1 v2) = show v1 ++" "++ show op ++" "++ show v2
    show (NewObj _ l) = "new "++show l
    show (NewArray _ t v) = "new "++show t++"["++show v++"]"
    --show (JumpCmp _ cmp l vl vr) = "    jump "++show l++" if "++show vl++" "++show cmp++" "++show vr
    --show (NewArray _ t v) = "    " ++ show t ++ " new array " ++ " = " ++ show v
    show (NewString _ l) = "    " ++ "new string " ++ show l
    --show (Val _ v) = "    val " ++ show v
    show (Call _ l args) = "    call<function> " ++ show l ++ "(" ++ intercalate ", " (map show args) ++ ")"
    show (MCall _ n cls i args) = "    call<method:" ++ show i ++ ", class:" ++ show cls ++ "> " ++ show n ++ "(" ++ intercalate ", " (map show args) ++ ")"
    --show (ArrAccess _ n v) = "    " ++ show name ++ "[" ++ show value ++ "]"
    --show (MemberAccess _ n off) = "    " ++ show n ++ "." ++ "(+" ++ show off ++ ")"
    show (Cast _ l val) = "    cast "++show val++" to "++show l
    show _ = "<unknown instruction>"

instance Show (Target a) where
    show (Variable _ v) = show v
    show (Array _ a v) = show a++"["++show v++"]"
    show( Member _ m cls field) = show m++".field["++show cls++"."++show field++"]"

instance Show (Type a) where
    show (IntT _) = "int"
    show (ByteT _) = "byte"
    show (Reference _ c) = "obj<" ++ show c ++ ">"
    show (ArrT _ c) = "array<" ++ show c ++ ">"

instance Show (Value a) where
    show (Const _ c) = show c
    show (Var _ x t) = "<" ++ show t ++ ">" ++ show x

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

getPos :: IsIR s => (s Position) -> Position
getPos ast = view (position @1) ast

getPosIR :: IsIR s => (s IRPosition) -> IRPosition
getPosIR ast = view (position @1) ast


setPosIR :: (IsIR s) => IRPosition -> (s IRPosition) -> (s IRPosition)
setPosIR p ast = set (position @1) p ast

-- modifyPos :: (IsIR s) => (Position -> IRPosition) -> (s Position) -> (s IRPosition)
-- modifyPos fn ast = let setPos p ast = set (position @1) p ast in ((setPos . fn . getPos) ast) ast

instance Eq (Method a) where
    (==) a b = (show $ fmap (const noPosIR) a) == (show $ fmap (const noPosIR) b)

instance Eq (Field a) where
    (==) a b = (show $ fmap (const noPosIR) a) == (show $ fmap (const noPosIR) b)

instance Eq (Name a) where
    (==) a b = (show $ fmap (const noPosIR) a) == (show $ fmap (const noPosIR) b)

instance Eq (Value a) where
    (==) a b = (show $ fmap (const noPosIR) a) == (show $ fmap (const noPosIR) b)

instance Eq (Type a) where
    (==) a b = (show $ fmap (const noPosIR) a) == (show $ fmap (const noPosIR) b)

instance Eq (Label a) where
    (==) a b = (show $ fmap (const noPosIR) a) == (show $ fmap (const noPosIR) b)

instance Eq (Stmt a) where
    (==) a b = (show $ fmap (const noPosIR) a) == (show $ fmap (const noPosIR) b)

instance Eq (Program a) where
    (==) a b = (show $ fmap (const noPosIR) a) == (show $ fmap (const noPosIR) b)

instance Eq (Constant a) where
    (==) a b = (show $ fmap (const noPosIR) a) == (show $ fmap (const noPosIR) b)

instance Eq (DataDef a) where
    (==) a b = (show $ fmap (const noPosIR) a) == (show $ fmap (const noPosIR) b)

instance Eq (Target a) where
    (==) a b = (show $ fmap (const noPosIR) a) == (show $ fmap (const noPosIR) b)

instance Eq (Entity a) where
    (==) a b = (show $ fmap (const noPosIR) a) == (show $ fmap (const noPosIR) b)

instance Eq (Function a) where
    (==) a b = (show $ fmap (const noPosIR) a) == (show $ fmap (const noPosIR) b)

instance Eq (Structure a) where
    (==) a b = (show $ fmap (const noPosIR) a) == (show $ fmap (const noPosIR) b)

instance Eq (Op a) where
    (==) a b = (show $ fmap (const noPosIR) a) == (show $ fmap (const noPosIR) b)

instance Eq (Cmp a) where
    (==) a b = (show $ fmap (const noPosIR) a) == (show $ fmap (const noPosIR) b)

instance Eq (Expr a) where
    (==) a b = (show $ fmap (const noPosIR) a) == (show $ fmap (const noPosIR) b)