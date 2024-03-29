{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE IncoherentInstances #-}
module Program.Syntax where

-- This module is kind of repeating the AST in a better way
-- Basically we want to convert the parsed tree
-- into this ProgramStructure to make some things easier
-- e.g. here we have a BinaryOp/UnaryOp rather then
-- specific operations like EMul/EAdd/Neg at the Expression level

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Control.Lens hiding(Empty)
import Data.Generics.Product
import Data.Generics.Sum
import GHC.Generics (Generic)

import Utils.Similarity

-- class WithPos a b where
--     pos :: a b -> b

class (HasPosition 1 (s t) (s t) t t, Show (s t)) => IsSyntax (s :: * -> *) t

data Ident a = Ident a String deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor)
instance (Show a) => IsSyntax Ident a

-- instance WithPos Ident a where
--     pos = view $ position @1

data OptionalName a = Name a (Ident a)
                | NoName a
  deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor)
instance (Show a) => IsSyntax OptionalName a

justName :: Ident a -> OptionalName a
justName id@(Ident pos _) = Name pos id

data Program a = Program a [Definition a]
  deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor)
instance (Show a) => IsSyntax Program a


data Definition a = FunctionDef a (Type a) (Ident a) [Arg a] (Block a)
                  | ClassDef a (Ident a) (OptionalName a) [ClassDecl a]
  deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor)
instance (Show a) => IsSyntax Definition a


data Arg a = Arg a (Type a) (Ident a)
  deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor)
instance (Show a) => IsSyntax Arg a


data Block a = Block a [Stmt a]
  deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor)
instance (Show a) => IsSyntax Block a


data ClassDecl a = FieldDecl a (Type a) (Ident a)
                 | MethodDecl a (Type a) (Ident a) [Arg a] (Block a)
  deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor)
instance (Show a) => IsSyntax ClassDecl a


data Stmt a = Empty a
            | BlockStmt a (Block a)
            | VarDecl a [(Type a, DeclItem a)]
            | Assignment a (Expr a) (Expr a)
            | ReturnValue a (Expr a)
            | ReturnVoid a
            | IfElse a (Expr a) (Stmt a) (Stmt a)
            | While a (Expr a) (Stmt a)
            | ExprStmt a (Expr a)
  deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor)
instance (Show a) => IsSyntax Stmt a


data DeclItem a = NoInit a (Ident a)
                | Init a (Ident a) (Expr a)
  deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor)
instance (Show a) => IsSyntax DeclItem a


data Type a = VoidT a
            | BoolT a
            | StringT a
            | IntT a
            | ByteT a
            | InfferedT a
            | ClassT a (Ident a)
            | ArrayT a (Type a)
            | FunT a (Type a) [Type a]
  deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor)
instance (Show a) => IsSyntax Type a

data SyntaxType = Implicit | Explicit deriving (Eq, Ord, Show, Read, Generic)

data Expr a = Lit a (Lit a)
            | Var a (Ident a)
            | App a (Expr a) [Expr a]         -- e1(e2)
            | UnaryOp a (UnOp a) (Expr a)
            | BinaryOp a SyntaxType (BinOp a) (Expr a) (Expr a)
            | Member a (Expr a) (Ident a) (Maybe TypeName)      -- (e1).e2
            | NewObj a (Type a) (Maybe (Expr a))               -- new T
            | ArrAccess a (Expr a) (Expr a) (Maybe (Type a))   -- e1[e2]
            | Cast a (Type a) (Expr a)         -- (T)e1
  deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor)
instance (Show a) => IsSyntax Expr a


type TypeName = String

data UnOp a = Neg a   --  -
            | Not a   --  !
  deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor)
instance (Show a) => IsSyntax UnOp a


data BinOp a = Add a
             | Sub a
             | Mul a
             | Div a
             | Mod a
             | Lt a
             | Le a
             | Equ a
             | Neq a
             | Gt a
             | Ge a
             | And a
             | Or a
  deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor)
instance (Show a) => IsSyntax BinOp a


data Lit a = Int a Integer
           | String a String
           | Bool a Bool
           | Byte a Integer
           | Null a
  deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor)
instance (Show a) => IsSyntax Lit a


makePrisms ''Ident
makePrisms ''OptionalName
makePrisms ''Program
makePrisms ''Definition
makePrisms ''Arg
makePrisms ''Block
makePrisms ''ClassDecl
makePrisms ''Stmt
makePrisms ''DeclItem
makePrisms ''Type
makePrisms ''Expr
makePrisms ''UnOp
makePrisms ''BinOp
makePrisms ''Lit

instance NearEq (Ident a) where
    similar (Ident _ x) (Ident _ y) = x == y
    similar _ _ = False

instance NearEq (OptionalName a) where
    similar (NoName _) (NoName _) = True
    similar (Name _ x) (Name _ y) = similar x y
    similar _ _ = False

instance NearEq (Type a) where
    similar (VoidT _) (VoidT _) = True
    similar (BoolT _) (BoolT _) = True
    similar (StringT _) (StringT _) = True
    similar (IntT _) (IntT _) = True
    similar (ByteT _) (ByteT _) = True
    similar (InfferedT _) (InfferedT _) = True
    similar (ClassT _ name1) (ClassT _ name2) = similar name1 name2
    similar (ArrayT _ t1) (ArrayT _ t2) = similar t1 t2
    similar (FunT _ r1 p1) (FunT _ r2 p2) = similar r1 r2 && similar p1 p2
    similar _ _ = False


instance PrettyPrint (Program a) where
    printi _ (Program _ defs) = intercalate "\n\n" (map (printi 0) defs)

instance PrettyPrint (Ident a) where
    printi _ (Ident _ s) = s

instance PrettyPrint (OptionalName a) where
    printi i (Name _ id) = printi i id
    printi _ (NoName _) = ""

printiOptionalName :: Int -> String -> String -> OptionalName a -> String
printiOptionalName i defaultValue prefix (Name _ id) = prefix ++ printi i id
printiOptionalName i defaultValue prefix (NoName _) = defaultValue

instance PrettyPrint (Definition a) where
    printi _ (FunctionDef _ t id args b) = printi 0 t ++ " " ++ printi 0 id ++ "(" ++ intercalate ", " (map (printi 0) args) ++ ")\n" ++ printi 1 b
    printi _ (ClassDef _ id mpar decls) = "class " ++ printi 0 id ++ (printiOptionalName 0 "" " extends " mpar) ++ "\n{\n" ++ intercalate "\n" (map (printi 1) decls) ++"\n}"

instance PrettyPrint (Block a) where
    printi i (Block _ stmts) = (replicate (i-1) '\t')++"{\n" ++ intercalate "\n" (map (printi i) stmts) ++ "\n"++(replicate (i-1) '\t')++"}"

instance PrettyPrint (Arg a) where
    printi _ (Arg _ t id) = printi 0 t ++ " " ++ printi 0 id

instance PrettyPrint (ClassDecl a) where
    printi i (FieldDecl _ t id) = (replicate i '\t')++printi 0 t ++ " " ++ printi 0 id ++ ";"
    printi i (MethodDecl _ t id args b) = (replicate i '\t')++printi 0 t ++ " " ++ printi 0 id ++ "(" ++ intercalate ", " (map (printi 0) args) ++ ")\n" ++ printi (i+1) b

instance PrettyPrint (Stmt a) where
    printi i (Empty _) = (replicate i '\t')++";"
    printi i (BlockStmt _ b) = printi (i+1) b
    printi i (VarDecl _ decls) = intercalate "\n" (map (\(t, d) -> (replicate i '\t')++printi 0 t ++ " " ++ printi 0 d ++ ";") decls)
    printi i (Assignment _ e1 e2) = (replicate i '\t')++printi 0 e1++" = "++printi 0 e2 ++ ";"
    printi i (ReturnValue _ e) = (replicate i '\t')++"return "++printi 0 e++";"
    printi i (ReturnVoid _) = (replicate i '\t')++"return;"
    printi i (IfElse _ e s1 s2) = (replicate i '\t')++"if ("++printi 0 e++")\n"++printi (i+1) s1 ++ "\n"++(replicate i '\t')++"else\n"++printi (i+1) s2
    printi i (While _ e s) = (replicate i '\t')++"while ("++printi 0 e++")\n"++printi (i+1) s
    printi i (ExprStmt _ e) = (replicate i '\t')++printi 0 e ++ ";"

instance PrettyPrint (DeclItem a) where
    printi _ (NoInit _ id) = printi 0 id
    printi _ (Init _ id e) = printi 0 id ++ " = "++ printi 0 e

instance PrettyPrint (Type a) where
    printi _ (VoidT _) = "void"
    printi _ (IntT _) = "int"
    printi _ (ByteT _) = "byte"
    printi _ (StringT _) = "string"
    printi _ (BoolT _) = "bool"
    printi _ (InfferedT _) = "var"
    printi _ (ClassT _ id) = printi 0 id
    printi _ (ArrayT _ t) = printi 0 t ++ "[]"
    printi _ (FunT _ t ts) = "("++printi 0 t++" ("++intercalate ", " (map (printi 0) ts) ++"))"

instance PrettyPrint (SyntaxType) where
    printi _ (Implicit) = "<implicit>"
    printi _ (Explicit) = ""

instance PrettyPrint (Expr a) where
    printi _ (Var _ id) = printi 0 id
    printi _ (Lit _ l) = printi 0 l
    printi _ (App _ e es) = printi 0 e ++ "("++intercalate ", " (map (printi 0) es)++")"
    printi _ (UnaryOp _ (Neg _) e) = "-"++printi 0 e
    printi _ (UnaryOp _ (Not _) e) = "!"++printi 0 e
    printi _ (BinaryOp _ skind op el er) = printi 0 skind++"("++printi 0 el ++" "++printi 0 op++" "++printi 0 er ++")"
    printi _ (Member _ e id Nothing) = printi 0 e ++"."++printi 0 id++"<???>"
    printi _ (Member _ e id (Just t)) = printi 0 e ++"."++printi 0 id++"<"++show t++">"
    printi _ (NewObj _ t m) =
        case m of
            Nothing -> "new "++printi 0 t
            Just e -> "new "++printi 0 t++"["++printi 0 e++"]"
    printi _ (ArrAccess _ e1 e2 _) = printi 0 e1 ++"["++printi 0 e2++"]"
    printi _ (Cast _ t e) = "("++printi 0 t++")("++printi 0 e++")"

instance PrettyPrint (BinOp a) where
    printi _ (Add _) = "+"
    printi _ (Sub _) = "-"
    printi _ (Mul _) = "*"
    printi _ (Div _) = "/"
    printi _ (Mod _) = "%"
    printi _ (Lt _) = "<"
    printi _ (Le _) = "<="
    printi _ (Equ _) = "=="
    printi _ (Neq _) = "!="
    printi _ (Gt _) = ">"
    printi _ (Ge _) = ">="
    printi _ (And _) = "&&"
    printi _ (Or _) = "||"

instance PrettyPrint (UnOp a) where
    printi _ (Not _) = "!"
    printi _ (Neg _) = "-"

instance PrettyPrint (Lit a) where
    printi _ (Int _ i) = show i
    printi _ (Byte _ i) = show i
    printi _ (String _ s) = show s
    printi _ (Bool _ b) = if b then "true" else "false"
    printi _ (Null _) = "null"


isBB (And _) = True
isBB (Or _) = True
isBB _ = False
isAA (Equ _) = True
isAA (Neq _) = True
isAA (Le _) = True
isAA (Lt _) = True
isAA (Ge _) = True
isAA (Gt _) = True
isAA _ = False

isCond (BinaryOp _ _ op _ _) = isBB op || isAA op
isCond _ = False

-- getPosE (Var a id) = a
-- getPosE (Lit a l) = a
-- getPosE (App a e es) = a
-- getPosE (UnaryOp a o e) = a
-- getPosE (BinaryOp a o e1 e2) = a
-- getPosE (Member a e id m) = a
-- getPosE (NewObj a t m) = a
-- getPosE (ArrAccess a el er m) = a
-- getPosE (Cast a t e) = a


-- getPos = (flip ^.) (position @1)
getPos :: IsSyntax s t => (s t) -> t
getPos k = view (position @1) k