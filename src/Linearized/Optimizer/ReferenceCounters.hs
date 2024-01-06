module Linearized.Optimizer.ReferenceCounters(run, initialState) where

import Data.List
import Control.Monad.State

import Control.Monad.Except hiding (void)
import Control.Monad.Reader hiding (void)
import Control.Monad.State hiding (void)

import qualified Linearized.Syntax as L
import qualified Linearized.Optimizer.Values as VP
import qualified Utils.Containers.IDMap as IM
import Linearized.Optimizer.Liveness
import Linearized.Def

import Data.List

import Reporting.Logs
import qualified Reporting.Errors.Def as Errors

type RCEnv = Int
type RCEnnricher a = LinearConverter RCEnv a

initialState :: RCEnv
initialState = 0

run :: L.Program L.IRPosition -> RCEnnricher (L.Program L.IRPosition)
run (L.Program p sts funs strs) = do
    let dupErr = (idMapFailure "addRefCount" Errors.ILNEDuplicateFunctionName)
    nfuncs <- IM.mapElemsM dupErr (\_ (L.Fun p l t args body) -> (\nbody -> L.Fun p l t args nbody) <$> addRefCounters (analize body) args) funs
    return (L.Program p sts nfuncs strs)

newVar :: L.IRPosition -> RCEnnricher (L.Name L.IRPosition)
newVar pos = do
    i <- oStateGet id
    oStateSet (+1)
    return $ L.Name pos $ "c"++show i
decr :: L.IRPosition -> L.Name L.IRPosition -> RCEnnricher (L.Stmt L.IRPosition)
decr pos name = do
    return (L.DecrCounter pos name)
incr :: L.IRPosition -> L.Name L.IRPosition -> RCEnnricher (L.Stmt L.IRPosition)
incr pos name = do
    return (L.IncrCounter pos name)

incArgs :: [LivenessEntry] -> [(L.Type L.IRPosition, L.Name L.IRPosition)] -> RCEnnricher ([L.Stmt L.IRPosition], [L.Name L.IRPosition])
incArgs sts ((L.Reference p, n):as) = do
    if isLive n then do
        i <- incr p n
        (rs, refs) <- incArgs sts as
        return $ (i : rs, n:refs)
    else incArgs sts as
    where
        isLive n = let (_,tin,_) = head sts in elem n tin
incArgs sts (_:as) = incArgs sts as
incArgs _ [] = return ([],[])

type ExLivenessEntry = (L.Stmt L.IRPosition, [L.Name L.IRPosition], [L.Name L.IRPosition], [L.Name L.IRPosition])

mapStmt :: [ExLivenessEntry] -> ExLivenessEntry -> [L.Name L.IRPosition] -> RCEnnricher ([L.Name L.IRPosition], [(L.Stmt L.IRPosition)])
mapStmt _ (s@(L.VarDecl p (L.Reference _) n e), tin, tout, dead) refs = do
    ds <- kill p dead refs
    i <- incr p n
    let ii = case e of
                L.Call _ _ _ -> []
                L.MCall _ _ _ _ -> []
                _ -> [i]
    if elem n tout then
        return $ (n:refs, ds ++ ii ++ [s])
    else do
        d <- decr p n
        return $ (n:refs, ds ++ d : ii ++ [s])
mapStmt _ (s@(L.Assign _ (L.Reference _) tg e), tin, tout, dead) refs = do
    case tg of
        L.Variable p v -> do
            ds <- kill p dead refs
            i <- incr p v
            let ii = case e of
                        L.Call _ _ _ -> []
                        L.MCall _ _ _ _ -> []
                        _ -> [i]
            return (refs, ds ++ ii ++ [s])
        L.Array p n v -> do
            ds <- kill p dead refs
            x <- newVar p
            let aX = L.VarDecl p (L.Reference p) x (L.ArrAccess p n v)
            dx <- decr p x
            let bX = L.Assign p (L.Reference p) (L.Variable p x) e
            ix <- incr p x
            let fin = L.Assign p (L.Reference p) tg (L.Val p (L.Var p x))
            let iix = case e of
                        L.Call _ _ _ -> []
                        L.MCall _ _ _ _ -> []
                        _ -> [ix]
            return $ (refs, ds ++ fin : iix ++ bX : dx : [aX])
        L.Member p n o -> do
            ds <- kill p dead refs
            x <- newVar p
            let aX = L.VarDecl p (L.Reference p) x (L.MemberAccess p n o)
            dx <- decr p x
            let bX = L.Assign p (L.Reference p) (L.Variable p x) e
            ix <- incr p x
            let fin = L.Assign p (L.Reference p) tg (L.Val p (L.Var p x))
            let iix = case e of
                        L.Call _ _ _ -> []
                        L.MCall _ _ _ _ -> []
                        _ -> [ix]
            return $  (refs, ds ++ fin : iix ++ bX : dx : [aX])
mapStmt _ (s@(L.ReturnVal p (L.Reference _) (L.Val _ (L.Const _ (L.Null _)))), tin, tout, dead) refs = do
    ds <- kill p dead refs
    return $ (refs, s : ds)
mapStmt _ (s@(L.ReturnVal p (L.Reference _) e), tin, tout, dead) refs = do
    ds <- kill p dead refs
    x <- newVar p
    let aX = L.VarDecl p (L.Reference p) x e
    i <- incr p x
    let ret = L.ReturnVal p (L.Reference p) (L.Val p (L.Var p x))
    let ii = case e of
                L.Call _ _ _ -> []
                L.MCall _ _ _ _ -> []
                _ -> [i]
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
mapStmt _ entry@((L.JumpCmp p _ _ _ _), tin, tout, dead) refs = killIfNoJump p entry refs
mapStmt sts (s@(L.SetLabel p l), tin, tout, dead) refs = do
    if isElseLabel l then do
        ds <- kill p dead refs
        let isJump = \s -> case s of
                            L.JumpCmp _ _ k _ _ -> l == k
                            _ -> False
        let ou = nub $ concat $ map (\(_,_,o,_)->o) $ filter (\(s,_,_,_) -> isJump s) sts
        let deadIfElse = ou \\ tout
        dds <- kill p deadIfElse refs
        return (refs, ds ++ dds ++ [s])
    else do
        ds <- kill p dead refs
        return (refs, ds ++ [s])
mapStmt _ (s, tin, tout, dead) refs = do
    ds <- kill (L.getPosIR s) dead refs
    return (refs, ds ++ [s])


kill :: L.IRPosition -> [L.Name L.IRPosition] -> [L.Name L.IRPosition] -> RCEnnricher [(L.Stmt L.IRPosition)]
kill p dead refs = do
    let deadrefs = filter (\d -> elem d refs) dead
    mapM (decr p) deadrefs

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
    (argsStmts, argsRefs) <- incArgs entries args
    let exEntries = map (\(s, tin, tout) -> (s, tin, tout, tin \\ tout)) entries
    (_, stmts) <- foldM (\(refs, outStmts) entry -> (\(newRefs, newStmts) -> (newRefs, newStmts ++ outStmts)) <$> (mapStmt exEntries entry refs)) (argsRefs, []) exEntries
    return stmts

-- addRefCounters sts args =
--   where
--     run :: RCEnnricher [L.Stmt L.IRPosition]
--     run = do
--         (a,r) <- incArgs args
--         walk sts r a
--     walk ((s,tin,tout):ss) refs acc = do
--         let dead = tin \\ tout
--         let pos = L.getPos s
--         ds <- kill pos dead refs
--         case s of
--             L.VarDecl p (L.Reference _) n e -> do
--                 i <- incr p n
--                 let ii = case e of
--                             L.Call _ _ _ -> []
--                             L.MCall _ _ _ _ -> []
--                             _ -> [i]
--                 if elem n tout then
--                     walk ss (n:refs) (ds ++ ii ++ s : acc)
--                 else do
--                     d <- decr p n
--                     walk ss (n:refs) (ds ++ d : ii ++ s : acc)
--             L.Assign _ (L.Reference _) tg e -> do
--                 case tg of
--                     L.Variable p v -> do
--                         i <- incr p v
--                         let ii = case e of
--                                     L.Call _ _ _ -> []
--                                     L.MCall _ _ _ _ -> []
--                                     _ -> [i]
--                         walk ss refs (ds ++ ii ++ s : acc)
--                     L.Array p n v -> do
--                         x <- newVar
--                         let aX = L.VarDecl (L.Reference p) x (L.ArrAccess p n v)
--                         dx <- decr p x
--                         let bX = L.Assign (L.Reference p) (L.Variable p x) e
--                         ix <- incr p x
--                         let fin = L.Assign (L.Reference p) tg (L.Val p (L.Var p x))
--                         let iix = case e of
--                                     L.Call _ _ _ -> []
--                                     L.MCall _ _ _ _ -> []
--                                     _ -> [ix]
--                         walk ss refs (ds ++ fin : iix ++ bX : dx : aX : acc)
--                     L.Member p n o -> do
--                         x <- newVar
--                         let aX = L.VarDecl p (L.Reference p) x (L.MemberAccess p n o)
--                         dx <- decr p x
--                         let bX = L.Assign p (L.Reference p) (L.Variable p x) e
--                         ix <- incr p x
--                         let fin = L.Assign p (L.Reference p) tg (L.Val p (L.Var p x))
--                         let iix = case e of
--                                     L.Call _ _ _ -> []
--                                     L.MCall _ _ _ _ -> []
--                                     _ -> [ix]
--                         walk ss refs (ds ++ fin : iix ++ bX : dx : aX : acc)
--             L.ReturnVal _ (L.Reference _) (L.Val _ (L.Const _ (L.Null _))) -> do
--                 walk ss refs (s : ds ++ acc)
--             L.ReturnVal p (L.Reference _) e -> do
--                 x <- newVar
--                 let aX = L.VarDecl (L.Reference p) x e
--                 i <- incr p x
--                 let ret = L.ReturnVal (L.Reference p) (L.Val (L.Var x))
--                 let ii = case e of
--                             L.Call _ _ _ -> []
--                             L.MCall _ _ _ _ -> []
--                             _ -> [i]
--                 walk ss refs (ret : ds ++ ii ++ aX : acc)
--             L.ReturnVal _ _ _ -> walk ss refs (s : ds ++ acc)
--             L.Return _ -> walk ss refs (s : ds ++ acc)
--             L.Jump _ _ -> walk ss refs (s : ds ++ acc)
--             L.JumpCmp p _ _ _ _ -> killIfNoJump p ss tin refs s ds acc dead
--             L.SetLabel p l ->
--                 if isElseLabel l then do
--                     let isJump = \s -> case s of
--                                         L.JumpCmp _ _ k _ _ -> l == k
--                                         _ -> False
--                     let ou = nub $ concat $ map (\(_,_,o)->o) $ filter (\(s,_,_) -> isJump s) sts
--                     let deadIfElse = ou \\ tout
--                     dds <- kill p deadIfElse refs
--                     walk ss refs (ds ++ dds ++ s : acc)
--                 else walk ss refs (ds ++ s :  acc)
--             _ -> walk ss refs (ds ++ s :  acc)
--     walk [] _ acc = return (reverse acc)
--     killIfNoJump p ss tin refs s ds acc dead = do
--         let deadIfNotJumped = filter (\i -> not $ elem i (let (_,ti,_) = head ss in ti)) tin \\ dead
--         dds <- kill p deadIfNotJumped refs
--         walk ss refs (dds ++ s : ds ++ acc)

--     kill p dead refs = do
--         let deadrefs = filter (\d -> elem d refs) dead
--         mapM (decr p) deadrefs
--     isElseLabel (a:b:c:d:_) = [a,b,c,d] == "_IEL" || [a,b,c,d] == "_WBE"
--     isElseLabel _ = False
