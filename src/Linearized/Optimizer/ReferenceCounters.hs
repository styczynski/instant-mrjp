{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Linearized.Optimizer.ReferenceCounters(run, initialState) where

import Data.List
import Control.Monad.State

import Control.Lens

import Control.Monad.Except hiding (void)
import Control.Monad.Reader hiding (void)
import Control.Monad.State hiding (void)

import Data.Maybe

import qualified Linearized.Syntax as L
import qualified Linearized.Optimizer.Values as VP
import qualified Utils.Containers.IDMap as IM
import qualified Data.Map as M
import Linearized.Optimizer.Liveness
import Linearized.Def

import Control.DeepSeq
import GHC.Generics (Generic)
import Data.List

import Reporting.Logs
import qualified Reporting.Errors.Def as Errors

type RCEnnricher a = LinearConverter RCEnv a

data RCEnv = RCEnv
  {
    _rcUniqueID :: Int 
    , _rcVarTypes :: M.Map String (L.Type L.IRPosition)
  } deriving (Show, Generic, NFData)

makeLensesWith abbreviatedFields ''RCEnv

initialState :: RCEnv
initialState = RCEnv {
    _rcUniqueID = 0
    , _rcVarTypes = M.empty
}

run :: L.Program L.IRPosition -> RCEnnricher (L.Program L.IRPosition)
run (L.Program p sts funs strs) = do
    let dupErr = (idMapFailure "addRefCount" Errors.ILNEDuplicateFunctionName)
    nfuncs <- IM.mapElemsM dupErr (\_ (L.Fun p lp l t args body) -> (\nbody -> L.Fun p lp l t args nbody) <$> addRefCounters (analize body) args) funs
    return (L.Program p sts nfuncs strs)

registerVarsOf :: L.Stmt L.IRPosition -> RCEnnricher (L.Stmt L.IRPosition)
registerVarsOf stmt@(L.VarDecl p vtype (L.Name _ name) _) = do
    liftPipelineOpt $ printLogInfoStr $ "Register var "++show name++" of type "++show vtype++" in stmt "++show stmt
    oStateSet (\env -> env & varTypes %~ M.insert name vtype)
    return stmt
registerVarsOf stmt@(L.Assign p vtype (L.Variable _ (L.Name _ name)) _) = do
    liftPipelineOpt $ printLogInfoStr $ "Register var "++show name++" of type "++show vtype++" in stmt "++show stmt
    oStateSet (\env -> env & varTypes %~ M.insert name vtype)
    return stmt
registerVarsOf stmt = return stmt

registerVar :: (L.Type L.IRPosition, L.Name L.IRPosition) -> RCEnnricher ()
registerVar (vType, L.Name _ name) = do
    liftPipelineOpt $ printLogInfoStr $ "Register var "++show name++" of type "++show vType++" just as-is"
    oStateSet (\env -> env & varTypes %~ M.insert name vType)

newVar :: L.IRPosition -> RCEnnricher (L.Name L.IRPosition)
newVar pos = do
    i <- oStateGet (^. uniqueID)
    oStateSet (\env -> env & uniqueID %~ (+1))
    return $ L.Name pos $ "c"++show i
decrByName :: L.IRPosition -> L.Name L.IRPosition -> RCEnnricher (L.Stmt L.IRPosition)
decrByName pos name@(L.Name _ nameStr) = do
    vTypes <- oStateGet (^. varTypes)
    case M.lookup nameStr vTypes of
        Nothing -> error $ "Cannot find var "++show nameStr++" in RC context: "++show vTypes
        Just t -> return (L.DecrCounter pos t name)
decr :: L.IRPosition -> L.Type L.IRPosition -> L.Name L.IRPosition -> RCEnnricher (L.Stmt L.IRPosition)
decr pos vtype name = do
    return (L.DecrCounter pos vtype name)
incr :: L.IRPosition -> L.Type L.IRPosition -> L.Name L.IRPosition -> RCEnnricher (L.Stmt L.IRPosition)
incr pos vtype name = do
    return (L.IncrCounter pos vtype name)

incArgs :: [LivenessEntry] -> [(L.Type L.IRPosition, L.Name L.IRPosition)] -> RCEnnricher ([L.Stmt L.IRPosition], [L.Name L.IRPosition])
incArgs sts ((L.Reference p l, n):as) = do
    if isLive n then do
        i <- incr p (L.Reference p l) n
        (rs, refs) <- incArgs sts as
        return $ (i : rs, n:refs)
    else incArgs sts as
    where
        isLive n = let (_,tin,_) = head sts in elem n tin
incArgs sts (_:as) = incArgs sts as
incArgs _ [] = return ([],[])

type ExLivenessEntry = (L.Stmt L.IRPosition, [L.Name L.IRPosition], [L.Name L.IRPosition], [L.Name L.IRPosition])

mapStmt :: [ExLivenessEntry] -> ExLivenessEntry -> [L.Name L.IRPosition] -> RCEnnricher ([L.Name L.IRPosition], [(L.Stmt L.IRPosition)])
mapStmt _ (s@(L.VarDecl p (L.Reference p' refType) n e), tin, tout, dead) refs = do
    registerVarsOf s
    ds <- kill p dead refs
    i <- incr p (L.Reference p' refType) n
    let ii = case e of
                L.Call _ _ _ -> []
                L.MCall _ _ _ _ _ -> []
                _ -> [i]
    if elem n tout then
        return $ (n:refs, ds ++ ii ++ [s])
    else do
        d <- decr p (L.Reference p' refType) n
        return $ (n:refs, ds ++ d : ii ++ [s])
mapStmt _ (s@(L.Assign _ (L.Reference p' refType) tg e), tin, tout, dead) refs = do
    registerVarsOf s
    case tg of
        L.Variable p v -> do
            i <- incr p (L.Reference p' refType) v
            let ii = case e of
                        L.Call _ _ _ -> []
                        L.MCall _ _ _ _ _ -> []
                        _ -> [i]
            ds <- kill p dead refs
            return (refs, ds ++ ii ++ [s])
        L.Array p n v -> do
            x <- newVar p
            aX <- registerVarsOf $ L.VarDecl p (L.Reference p' refType) x (L.ArrAccess p n v)
            dx <- decr p (L.Reference p' refType) x
            let bX = L.Assign p (L.Reference p' refType) (L.Variable p x) e
            ix <- incr p (L.Reference p' refType) x
            let fin = L.Assign p (L.Reference p' refType) tg (L.Val p (L.Var p x (L.Reference p' refType)))
            let iix = case e of
                        L.Call _ _ _ -> []
                        L.MCall _ _ _ _ _ -> []
                        _ -> [ix]
            ds <- kill p dead refs
            return $ (refs, ds ++ fin : iix ++ bX : dx : [aX])
        L.Member p n cls mem -> do
            x <- newVar p
            aX <- registerVarsOf $ L.VarDecl p (L.Reference p' refType) x (L.MemberAccess p n cls mem (L.Reference p' refType))
            dx <- decr p (L.Reference p' refType) x
            let bX = L.Assign p (L.Reference p' refType) (L.Variable p x) e
            ix <- incr p (L.Reference p' refType) x
            let fin = L.Assign p (L.Reference p' refType) tg (L.Val p (L.Var p x (L.Reference p' refType)))
            let iix = case e of
                        L.Call _ _ _ -> []
                        L.MCall _ _ _ _ _ -> []
                        _ -> [ix]
            ds <- kill p dead refs
            return $  (refs, ds ++ fin : iix ++ bX : dx : [aX])
mapStmt _ (s@(L.ReturnVal p (L.Reference _ _) (L.Val _ (L.Const _ (L.Null _ _)))), tin, tout, dead) refs = do
    ds <- kill p dead refs
    return $ (refs, s : ds)
mapStmt _ (s@(L.ReturnVal p (L.Reference p' refType) e), tin, tout, dead) refs = do
    x <- newVar p
    aX <- registerVarsOf $ L.VarDecl p (L.Reference p' refType) x e
    i <- incr p (L.Reference p' refType) x
    let ret = L.ReturnVal p (L.Reference p' refType) (L.Val p (L.Var p x (L.Reference p' refType)))
    let ii = case e of
                L.Call _ _ _ -> []
                L.MCall _ _ _ _ _ -> []
                _ -> [i]
    ds <- kill p dead refs
    return $ (refs, ret : ds ++ ii ++ [aX])
mapStmt _ (s@(L.ReturnVal p _ _), tin, tout, dead) refs = do
    ds <- kill p dead refs
    return (refs, s : ds)
mapStmt _ (s@(L.Return p), tin, tout, dead) refs = do
    ds <- kill p dead refs
    return (refs, s : ds)
mapStmt _ (s@(L.Jump p _), tin, tout, dead) refs = do
    ds <- kill p dead refs
    return (refs, s : ds)
mapStmt _ entry@((L.JumpCmp p _ _ _ _ _), tin, tout, dead) refs = killIfNoJump p entry refs
mapStmt sts (s@(L.SetLabel p l), tin, tout, dead) refs = do
    if isElseLabel l then do
        ds <- kill p dead refs
        let isJump = \s -> case s of
                            L.JumpCmp _ _ k _ _ _ -> l == k
                            _ -> False
        let ou = nub $ concat $ map (\(_,_,o,_)->o) $ filter (\(s,_,_,_) -> isJump s) sts
        let deadIfElse = ou \\ tout
        dds <- kill p deadIfElse refs
        return (refs, ds ++ dds ++ [s])
    else do
        ds <- kill p dead refs
        return (refs, ds ++ [s])
mapStmt _ (s, tin, tout, dead) refs = do
    registerVarsOf s
    ds <- kill (L.getPosIR s) dead refs
    return (refs, ds ++ [s])


kill :: L.IRPosition -> [L.Name L.IRPosition] -> [L.Name L.IRPosition] -> RCEnnricher [(L.Stmt L.IRPosition)]
kill p dead refs = do
    let deadrefs = filter (\d -> elem d refs) dead
    mapM (decrByName p) deadrefs

killIfNoJump :: L.IRPosition -> ExLivenessEntry -> [L.Name L.IRPosition] -> RCEnnricher ([L.Name L.IRPosition], [(L.Stmt L.IRPosition)])
killIfNoJump p (s, tin, tout, dead) refs = do
    ds <- kill p dead refs
    let deadIfNotJumped = filter (\i -> not $ elem i tin) tin \\ dead
    dds <- kill p deadIfNotJumped refs
    return (refs, dds ++ [s] ++ ds)
    --walk ss refs (dds ++ s : ds ++ acc)

isElseLabel :: (L.Label L.IRPosition) -> Bool
isElseLabel (L.Label _ name) = any ((flip isPrefixOf) name) ["_IEL", "_WBE"]
isElseLabel _ = False


addRefCounters :: [LivenessEntry] -> [(L.Type L.IRPosition, L.Name L.IRPosition)] -> RCEnnricher [L.Stmt L.IRPosition]
addRefCounters entries args = do
    liftPipelineOpt $ printLogInfoStr $ "addRefCounters for new function!"
    oStateSet (\env -> env & varTypes .~ M.empty)
    mapM_ registerVar args
    (argsStmts, argsRefs) <- incArgs entries args
    let exEntries = map (\(s, tin, tout) -> (s, tin, tout, tin \\ tout)) entries
    (_, stmts) <- foldM (\(refs, outStmts) entry -> (\(newRefs, newStmts) -> (newRefs, newStmts ++ outStmts)) <$> (mapStmt exEntries entry refs)) (argsRefs, []) exEntries
    return $ reverse stmts

