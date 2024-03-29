module IR.Size where

import           Data.Int
import           IR.Syntax.Syntax

import qualified Backend.X64.Parser.Constructor as X64

typeSize :: SType a -> X64.Size
typeSize t = case t of
    Int _   -> X64.Size32
    Bool _  -> X64.Size8
    Ref _ _ -> X64.Size64
    (Cl _ _) -> X64.Size64
    _       -> error $ "typeSize: invalid type " ++ show (() <$ t)

valSize :: Val a -> X64.Size
valSize val = case val of
    VInt _  _    -> X64.Size32
    VNegInt _  _ -> X64.Size32
    VTrue _      -> X64.Size8
    VFalse _     -> X64.Size8
    VNull {}     -> X64.Size64
    VVal _ t _   -> typeSize t
