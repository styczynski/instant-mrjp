module IR.Types where

import           IR.Syntax.Syntax

defaultVal :: SType a -> Val ()
defaultVal t = case deref t of
    Int _   -> VInt () 0
    Bool _  -> VFalse ()
    Cl _ _  -> VNull () (() <$ t)
    Arr _ _ -> VNull () (() <$ t)
    _       -> error $ "defaultVal: invalid type " ++ show (() <$ t)

deref :: SType a -> SType a
deref t = case t of
    Ref _ t' -> t'
    _        -> t

isInt :: SType a -> Bool
isInt t = case deref t of
    Int _ -> True
    _     -> False

isRef :: SType a -> Bool
isRef t = case t of
    Ref {} -> True
    _      -> False

isStr :: SType a -> Bool
isStr t = (() <$ deref t) == strType

strType :: SType ()
strType = Cl () (IRTargetRefName "String")

-- toSType :: Latte.Type a -> SType a
-- toSType t = case t of
--     Latte.Int a                -> Int a
--     Latte.Bool a               -> Bool a
--     Latte.Void a               -> Void a
--     Latte.Var {}               -> error "toSType: not a simple type 'var'"
--     Latte.Arr a t'             -> Ref a (Arr a (toSType t'))
--     Latte.Cl a i               -> Ref a (Cl a (toIRTargetRefName i))
--     Latte.Fun{}                -> error "toSType: not a simple type Fun"
--     Latte.Ref _ (Latte.Int a)  -> Int a
--     Latte.Ref _ (Latte.Bool a) -> Bool a
--     Latte.Ref a t'             -> Ref a (deref $ toSType t')

-- toFType :: Latte.Type a -> FType a
-- toFType t = case t of
--     Latte.Fun a r ps -> FType a (toSType r) (map toSType ps)
--     _                -> error "not a function type"

valType :: Val a -> SType a
valType val = case val of
    VInt a _    -> Int a
    VNegInt a _ -> Int a
    VTrue a     -> Bool a
    VFalse a    -> Bool a
    VNull a t   -> Ref a t
    VVal _ t _  -> t

ptrType :: Ptr a -> SType a
ptrType ptr = case ptr of
    PArrLen a _    -> Int a
    PElem _ t _ _  -> t
    PFld _ t _ _   -> t
    PParam _ t _ _ -> t
    PLocal _ t _   -> t