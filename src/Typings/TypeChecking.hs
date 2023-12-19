module Typings.TypeChecking where

import qualified Program.Syntax as Syntax
import Typings.Def
import Typings.Types as Type
import Reporting.Errors.Position

isParent :: [Type.Class] -> Syntax.Ident Position -> Syntax.Ident Position -> TypeChecker Bool
isParent classes idSon idPar = do
    let h = hierarchy classes idSon
        h' = map (\(Type.Class id _ _ _) -> id) h
    elemH idPar h'
  where
    elemH id@(Syntax.Ident _ n1) ((Syntax.Ident _ n2):xs) =
        if n1 == n2 then return True
        else elemH id xs
    elemH _ [] = return False

tcanBeCastUp :: Syntax.Type Position -> Syntax.Type Position -> TypeChecker Bool
tcanBeCastUp tFrom tTo = do
    case (tFrom, tTo) of
        (Syntax.IntT _, Syntax.IntT _) -> return True
        (Syntax.ByteT _, Syntax.ByteT _) -> return True
        (Syntax.BoolT _, Syntax.BoolT _) -> return True
        (Syntax.StringT _, Syntax.StringT _) -> return True
        (Syntax.VoidT _, Syntax.VoidT _) -> return True
        (Syntax.ByteT _, Syntax.IntT _) -> return True
        (Syntax.StringT _, Syntax.ClassT _ (Syntax.Ident _ "String")) -> return True
        (Syntax.StringT _, Syntax.ClassT _ (Syntax.Ident _ "Object")) -> return True
        (Syntax.ArrayT _ _, Syntax.ClassT _ (Syntax.Ident _ "Object")) -> return True
        (Syntax.ClassT _ (Syntax.Ident _ "String"), Syntax.StringT _) -> return True
        (Syntax.ClassT _ idSon, Syntax.ClassT _ idPar) -> if idSon == idPar then return True
                                          else isParent idSon idPar
        (Syntax.ArrayT _ t1, Syntax.ArrayT _ t2) -> equivalentType t1 t2
        (Syntax.FunT _ t1 ts1, Syntax.FunT _ t2 ts2) -> do
            t <- tcanBeCastUp t1 t2
            let lcheck = length ts1 == length ts2
            cs <- mapM (\(t1, t2) -> tcanBeCastUp classes t1 t2) (zip ts1 ts2)
            return (all id (lcheck:t:cs))
        (Syntax.InfferedT _, _) -> return True -- only when checking Expr.App
        (Syntax.StringT _, Syntax.InfferedT _) -> return True -- only when casting null
        (Syntax.ClassT _ _, Syntax.InfferedT _) -> return True -- only when casting null
        (Syntax.ArrayT _ _, Syntax.InfferedT _) -> return True -- only when casting null
        _ -> return False

canBeCastDown :: Syntax.Type Position -> Syntax.Type Position -> TypeChecker Bool
canBeCastDown tFrom tTo = 
    case (tFrom, tTo) of
        (Syntax.IntT _, Syntax.ByteT _) -> return True
        (Syntax.ByteT _, Syntax.IntT _) -> return True
        (Syntax.ClassT _ idFrom, Syntax.ClassT _ idTo) -> do
            (classes, _, _) <- ask
            b1 <- lift $ isParent classes idTo idFrom
            b2 <- lift $ isParent classes idFrom idTo
            return (b1 || b2)
        (Syntax.ClassT _ (Syntax.Ident _ "Object"), Syntax.ArrayT _ _) -> return True
        _ -> canBeCastUp tTo tFrom

equivalentType :: t -> Syntax.Type a -> Syntax.Type a -> TypeChecker Bool
equivalentType cls t1 t2 = do
    a <- tcanBeCastUp cls t1 t2
    b <- tcanBeCastUp cls t2 t1
    return (a && b)