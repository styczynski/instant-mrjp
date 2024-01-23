{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Linearized.Optimizer.ValuePropagator(run, initialState) where
import Data.List
import Control.Monad.State
import Control.Lens

import Control.Monad.Except hiding (void)
import Control.Monad.Reader hiding (void)
import Control.Monad.State hiding (void)

import qualified Linearized.Syntax as L
import qualified Linearized.Optimizer.Values as VP
import qualified Utils.Containers.IDMap as IM
import Linearized.Optimizer.Liveness
import Linearized.Def
import Control.DeepSeq
import GHC.Generics (Generic)

import Data.List
import Data.Maybe

import Reporting.Logs
import qualified Reporting.Errors.Def as Errors

unknownType = L.Reference L.noPosIR $ L.Label L.noPosIR "?"

data VPEnv = VPEnv
  {
    _vpValuesMap :: IM.Map (L.Name L.IRPosition, L.Value L.IRPosition)
  } deriving (Show, Generic, NFData)

makeLensesWith abbreviatedFields ''VPEnv

type ValuePropagator a = LinearConverter VPEnv a

initialState :: VPEnv
initialState = VPEnv {
    _vpValuesMap = IM.empty
}

run :: L.Program L.IRPosition -> ValuePropagator (L.Program L.IRPosition)
run (L.Program p sts funs strs) = do
    let dupErr = (idMapFailure "ValuePropagator" Errors.ILNEDuplicateFunctionName)
    nfuncs <- IM.mapElemsM dupErr (\_ (L.Fun p cls l t args body) -> (\nbody -> return $ L.Fun p cls l t args nbody) =<< removeUnusedStmts =<< propagateValuesStmt body) funs
    return (L.Program p sts nfuncs strs)

propE :: (L.Expr L.IRPosition) -> ValuePropagator (L.Expr L.IRPosition)
propE (L.NewArray p t v) = do
    v' <- updatedVal v
    return (L.NewArray p t v')
propE (L.Val p v) = do
    v' <- updatedVal v
    return (L.Val p v')
propE (L.Call p l vs) = do
    vs' <- mapM updatedVal vs
    return (L.Call p l vs')
propE (L.MCall p n cls idx vs) = do
    vs' <- mapM updatedVal vs
    (L.Var _ m _) <- updatedVal (L.Var p n unknownType)
    return (L.MCall p m cls idx vs')
propE (L.ArrAccess p n v) = do
    v' <- updatedVal v
    (L.Var _ m _) <- updatedVal (L.Var p n unknownType)
    return (L.ArrAccess p m v')
propE (L.IntToByte p v) = updatedVal v >>= return . L.IntToByte p
propE (L.ByteToInt p v) = updatedVal v >>= return . L.ByteToInt p
propE (L.Not p v) = updatedVal v >>= return . L.Not p
propE (L.BinOp p op v1 v2) = do
    v1' <- updatedVal v1
    v2' <- updatedVal v2
    case (op, v1, v2) of
        (L.Add _, (L.Const _ (L.IntC _ 0)), _) -> return (L.Val p v2)
        (L.Add _, _, (L.Const _ (L.IntC _ 0))) -> return (L.Val p v1)
        (L.Sub _, _, (L.Const _ (L.IntC _ 0))) -> return (L.Val p v1)
        (L.Mul _, (L.Const _ (L.IntC _ 1)), _) -> return (L.Val p v2)
        (L.Mul _, _, (L.Const _ (L.IntC _ 1))) -> return (L.Val p v1)
        _ -> return (L.BinOp p op v1' v2')
propE (L.MemberAccess p n cls member fieldType) = do
    (L.Var _ m _) <- updatedVal (L.Var p n unknownType)
    return (L.MemberAccess p m cls member fieldType)
propE (L.Cast p l v) = updatedVal v >>= return . L.Cast p l
propE e = return e

removeUnusedStmts :: [L.Stmt L.IRPosition] -> ValuePropagator ([L.Stmt L.IRPosition])
removeUnusedStmts stmts =
    let used' = foldl (\a s -> VP.used' s ++ a) [] stmts
        filtered = filter (isUsedStmt used') stmts
        usedFiltered = foldl (\a s -> VP.used s ++ a) [] filtered
        final = map (transformUnusedStmt usedFiltered) filtered
    in return final
    where
        transformUnusedStmt :: [L.Name L.IRPosition] -> (L.Stmt L.IRPosition) -> (L.Stmt L.IRPosition)
        transformUnusedStmt u stmt@(L.VarDecl _ t n (L.Call p l v)) = if elem n u then stmt else (L.VCall p t l v)
        transformUnusedStmt u stmt@(L.VarDecl _ t n (L.MCall p cn cls l v)) = if elem n u then stmt else (L.VMCall p t cn cls l v)
        transformUnusedStmt _ stmt = stmt
        isUsedStmt :: [L.Name L.IRPosition] -> (L.Stmt L.IRPosition) -> Bool
        isUsedStmt u (L.VarDecl _ _ n _) = elem n u
        isUsedStmt u (L.Assign _ _ (L.Variable _ n) _) = elem n u
        isUsedStmt _ _ = True

propagateValuesStmt :: [L.Stmt L.IRPosition] -> ValuePropagator ([L.Stmt L.IRPosition])
propagateValuesStmt (L.VarDecl p t n e : L.VarDecl p' t' n' (L.Val _ (L.Var _ m _)) : ss) | m == n && (not $ elem n $ concat $ map VP.used ss) && notFix n =
        propagateValuesStmt $ (L.VarDecl p t n' e : ss)
propagateValuesStmt (L.VarDecl p t n e : L.Assign p' t' (L.Variable p'' n') (L.Val _ (L.Var _ m _)) : ss) | m == n && (not $ elem n $ concat $ map VP.used ss) && notFix n =
        propagateValuesStmt $ (L.Assign p' t (L.Variable p'' n') e : ss)
propagateValuesStmt (L.VarDecl p t n (L.Val _ _) : L.Assign _ t' (L.Variable _ n') e : ss) | n == n' =
        propagateValuesStmt $ (L.VarDecl p t n e : ss)
propagateValuesStmt (L.VarDecl p t n e : ss) = do
    e' <- propE e
    case e' of
        (L.Val _ v) ->
            case v of
                (L.Var _ n _) -> do 
                    let assignedInFuture = concat $ map VP.assigned ss
                    if elem n assignedInFuture then return ()
                    else addVar v n
                _ -> addVar v n
        _ -> return ()
    next <- propagateValuesStmt ss
    return $ [L.VarDecl p t n e'] ++ next
propagateValuesStmt (L.Assign p t tg e : ss) = do
    e' <- propE e
    case tg of
        (L.Variable _ n) -> do
            removeVar n
            next <- propagateValuesStmt ss
            return $ [L.Assign p t tg e'] ++ next
        (L.Array p' n v) -> do
            v' <- updatedVal v
            next <- propagateValuesStmt ss
            return $ [L.Assign p t (L.Array p' n v') e'] ++ next
        _ -> do
            next <- propagateValuesStmt ss
            return $ [L.Assign p t tg e'] ++ next
propagateValuesStmt (L.ReturnVal p t e : ss) = do
    e' <- propE e
    next <- propagateValuesStmt ss
    return $ [L.ReturnVal p t e'] ++ next
propagateValuesStmt (L.JumpCmp p cmp l lpass vl vr : ss) = do
    vl' <- updatedVal vl
    vr' <- updatedVal vr
    next <- propagateValuesStmt ss
    return $ [L.JumpCmp p cmp l lpass vl' vr'] ++ next
propagateValuesStmt (s@(L.SetLabel p (L.Label _ ('_':'W':_))) : ss) = do
    let assignedInFuture = concat $ map VP.assigned ss
    mapM_ removeVar assignedInFuture
    next <- propagateValuesStmt ss
    return $ [s] ++ next
propagateValuesStmt (s@(L.SetLabel p (L.Label _ ('_':'I':_))) : ss) = do
    let assignedInFuture = concat $ map VP.assigned ss
    mapM_ removeVar assignedInFuture
    next <- propagateValuesStmt ss
    return $ [s] ++ next
propagateValuesStmt (s:ss) = do
    next <- propagateValuesStmt ss
    return $ [s] ++ next
propagateValuesStmt [] = return []

updatedVal :: (L.Value L.IRPosition) -> ValuePropagator (L.Value L.IRPosition)
updatedVal v@(L.Var p (L.Name _ n) _) = (maybe (return v) (return . L.setPosIR p . snd)) =<< oStateGet ((IM.lookup n) . (^. valuesMap))
updatedVal v = return v

addVar :: (L.Value L.IRPosition) -> (L.Name L.IRPosition) -> ValuePropagator ()
addVar val name@(L.Name _ n) = do --join $ oStateSet (\env -> env & valuesMap %~ IM.insertM (idMapFailure "ValuePropagator.addVar" Errors.ILNEDuplicateValueMapping) (name, val))
    newValues <- (IM.insertM (idMapFailure "ValuePropagator.addVar" Errors.ILNEDuplicateValueMapping) (name, val)) =<< (return . IM.delete n) =<< oStateGet (^. valuesMap)
    oStateSet (\env -> env & valuesMap .~ newValues)

removeVar :: (L.Name L.IRPosition) -> ValuePropagator ()
removeVar (L.Name _ name) = oStateSet (\env -> env & valuesMap %~ (IM.delete name))

notBinOp :: (L.Expr L.IRPosition) -> Bool
notBinOp (L.BinOp _ _ _ _) = False
notBinOp _ = True

notFix :: (L.Name L.IRPosition) -> Bool
notFix (L.Name _ name) = isSuffixOf "_f" name