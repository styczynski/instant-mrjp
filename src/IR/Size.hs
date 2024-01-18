module IR.Size where

import           Data.Int
import           IR.Syntax.Syntax

import qualified Backend.X64.Parser.Constructor as X64

--data Size = Byte | Double | Quadruple deriving (Eq, Show, Ord)

sizeInBytes :: X64.Size -> Int64
sizeInBytes size = case size of
    X64.Size8      -> 1
    X64.Size32    -> 4
    X64.Size64 -> 8

typeSize :: SType a -> X64.Size
typeSize t = case t of
    Int _   -> X64.Size32
    Bool _  -> X64.Size8
    Ref _ _ -> X64.Size64
    _       -> error $ "typeSize: invalid type " ++ show (() <$ t)

valSize :: Val a -> X64.Size
valSize val = case val of
    VInt _  _    -> X64.Size32
    VNegInt _  _ -> X64.Size32
    VTrue _      -> X64.Size8
    VFalse _     -> X64.Size8
    VNull {}     -> X64.Size64
    VVal _ t _   -> typeSize t
