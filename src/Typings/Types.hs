{-# LANGUAGE FlexibleInstances #-}
module Typings.Types where

import Reporting.Errors.Position
import qualified Program.Syntax as Syntax
import Utils.Similarity
--import qualified Error.Diagnose as Syntax

data AllowVoid = AllowVoid | NoVoid

data Class = Class 
                {-name-}(Name) 
                {-parent-}(Syntax.OptionalName Position)
                {-members-}[Member]
                (Syntax.Definition Position)
    deriving (Eq, Ord, Show)
data Member = Field (Name) (Type) (Syntax.ClassDecl Position)
            | Method (Name) (Type) [Type] (Syntax.ClassDecl Position)
    deriving (Eq, Ord, Show)
data Function  = Fun (Name) Type [Type] (Syntax.Definition Position)
    deriving (Eq, Show)

type Type = Syntax.Type Position

type Name = Syntax.Ident Position

-- instance NearEq (Syntax.Type t) where
--     similar a b = (Undefined <$ a) == (Undefined <$ b)

instance NearEq Function where
    similar (Fun _ _ _ decl1) (Fun _ _ _ decl2) = similar decl1 decl2
    similar _ _ = False

instance NearEq Member where
    similar (Field  _ _ decl1) (Field _ _ decl2) = similar decl1 decl2
    similar (Method _ _ _ decl1) (Method _ _ _ decl2) = similar decl1 decl2
    similar _ _ = False

instance NearEq Class where
    similar (Class _ _ _ decl1) (Class _ _ _ decl2) = similar decl1 decl2
    similar _ _ = False

class TypeContext a where
    nameOf :: a -> Name
    namePosition :: a -> Position
    location :: a -> Position
    location a = let (Syntax.Ident pos _) = nameOf a in pos
    stringName :: a -> String
    stringName a = let (Syntax.Ident _ id) = nameOf a in id

instance TypeContext Name where
    nameOf = id
    namePosition (Syntax.Ident pos _) = pos

instance TypeContext Class where
    nameOf (Class name _ _ _) = name
    location (Class _ _ _ (Syntax.ClassDef pos _ _ _)) = pos

    namePosition (Class _ _ _ (Syntax.ClassDef _ (Syntax.Ident pos _) _ _)) = pos

instance TypeContext Member where
    nameOf (Field name _ _) = name
    nameOf (Method name _ _ _) = name

    location (Field _ _ (Syntax.FieldDecl pos _ _)) = pos
    location (Method _ _ _ (Syntax.MethodDecl pos _ _ _ _)) = pos

    namePosition (Field _ _ (Syntax.FieldDecl _ _ (Syntax.Ident pos _))) = pos
    namePosition (Method _ _ _ (Syntax.MethodDecl _ _ (Syntax.Ident pos _) _ _)) = pos

instance TypeContext Function where
    nameOf (Fun name _ _ _) = name
    location (Fun _ _ _ (Syntax.FunctionDef pos _ _ _ _)) = pos
    namePosition (Fun _ _ _ (Syntax.FunctionDef _ _ (Syntax.Ident pos _) _ _)) = pos

extendsPosition :: Class -> Position
extendsPosition (Class _ _ _ (Syntax.ClassDef _ _ (Syntax.Name _ (Syntax.Ident pos _)) _)) = pos
--extendsPosition (Class _ _ _ (Syntax.ClassDef _ _ (Syntax.NoName pos) _)) = pos
extendsPosition cls = location cls

classMembers :: Class -> [Member]
classMembers (Class _ _ members _) = members
