{-# LANGUAGE FlexibleInstances #-}
module Typings.Types where

import Reporting.Errors.Position
import qualified Utils.Containers.IDMap as IM
import qualified Program.Syntax as Syntax
import Utils.Similarity
import Data.Maybe
--import qualified Error.Diagnose as Syntax

data AllowVoid = AllowVoid | NoVoid

data Class = Class
                {-name-}(Name)
                {-parent-}(Syntax.OptionalName Position)
                {-members-}[Member]
                (Syntax.Definition Position)
    deriving (Eq, Ord, Show)
data Member = Field (Name) (Type) (Syntax.ClassDecl Position)
            | Method (Name) (Type) (IM.Map (Name, Type)) (Syntax.ClassDecl Position)
    deriving (Eq, Ord, Show)
data Function  = Fun (Name) Type (IM.Map (Name, Type)) (Syntax.Definition Position)
    deriving (Eq, Show)

type Type = Syntax.Type Position

type Name = Syntax.Ident Position

instance IM.Idable (Syntax.Ident Position, Type) where
    getID ((Syntax.Ident _ name), _) = name


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
    location (Fun _ _ _ (Syntax.ClassDef pos _ _ _)) = pos

    namePosition (Fun _ _ _ (Syntax.FunctionDef _ _ (Syntax.Ident pos _) _ _)) = pos
    namePosition f = location f

extendsPosition :: Class -> Position
extendsPosition (Class _ _ _ (Syntax.ClassDef _ _ (Syntax.Name _ (Syntax.Ident pos _)) _)) = pos
--extendsPosition (Class _ _ _ (Syntax.ClassDef _ _ (Syntax.NoName pos) _)) = pos
extendsPosition cls = location cls

findClassMember :: String -> Class -> Maybe Member
findClassMember name = listToMaybe . filter (\member -> stringName member == name) . classMembers

classMembers :: Class -> [Member]
classMembers (Class _ _ members _) = members

funcArgsTypes :: Function -> [Type]
funcArgsTypes (Fun _ _ args _) = map snd $ IM.elems args

methodArgsTypes :: Member -> [Type]
methodArgsTypes (Method _ _ args _) = map snd $ IM.elems args
methodArgsTypes _ = []

memberType :: Member -> Type
memberType (Method (Syntax.Ident p _) t ts _) = Syntax.FunT p t $ map (snd . snd) $ IM.mapList (\key arg -> (key, arg)) ts
memberType (Field _ t _) = t

-- named n (Method (Ident _ nn) _ _) = n == nn
-- named n (Field (Ident _ nn) _) = n == nn
-- memberType (Method (Ident p _) t ts) = FunT p t ts
-- memberType (Field (Ident p _) t) = t