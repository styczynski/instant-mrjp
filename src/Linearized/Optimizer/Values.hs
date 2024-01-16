module Linearized.Optimizer.Values where

import qualified Linearized.Syntax as L

class (L.IsIR ma) => WithVariables ma where
    used :: (ma a) -> [L.Name a]
    used' :: (ma a) -> [L.Name a]
    assigned :: (ma a) -> [L.Name a]
    declared :: (ma a) -> [L.Name a]

    used' = used

instance WithVariables L.Stmt where
    used (L.VarDecl _ t n e) = used e
    used (L.Assign _ t tg e) = used tg ++ used e
    used (L.ReturnVal _ t e) = used e
    used (L.IncrCounter _ n) = [n]
    used (L.DecrCounter _ n) = [n]
    used (L.JumpCmp _ _ _ vl vr) = used vl ++ used vr
    used (L.VCall _ _ _ vs) = concat $ map used vs
    used (L.VMCall _ _ n cls i vs) = concat $ map used vs
    used _ = []

    used' (L.VarDecl _ t n e@(L.Call _ _ _)) = n : used e
    used' (L.VarDecl _ t n e@(L.MCall _ _ _ _ _)) = n : used e
    used' e = used e
    
    assigned (L.VarDecl _ _ n _) = [n]
    assigned (L.Assign _ t (L.Variable _ n) _) = [n]
    assigned _ = []

    declared (L.VarDecl _ _ n _) = [n]
    declared _ = []

instance WithVariables L.Target where
    used (L.Array _ n v) = n : used v
    used (L.Member _ n _ _) = [n]
    used _ = []

    assigned _ = []
    declared _ = []

instance WithVariables L.Value where
    used (L.Var _ n _) = [n]
    used _ = []
    
    assigned _ = []
    declared _ = []

instance WithVariables L.Expr where
    used (L.NewArray _ _ v) = used v
    used (L.Val _ v) = used v
    used (L.Call _ _ vs) = concat $ map used vs
    used (L.MCall _ n i cls vs) = concat $ map used vs
    used (L.ArrAccess _ n v) = n : used v
    used (L.MemberAccess _ n _ _ _) = [n]
    used (L.IntToByte _ v) = used v
    used (L.ByteToInt _ v) = used v
    used (L.Not _ v) = used v
    used (L.BinOp _ _ v1 v2) = used v1 ++ used v2
    used (L.Cast _ _ v) = used v
    used _ = []

    assigned _ = []
    declared _ = []

