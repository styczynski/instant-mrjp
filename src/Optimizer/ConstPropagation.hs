{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Optimizer.ConstPropagation(
    run
    , initialState
) where

import Control.Lens

import qualified Data.Map as M
import qualified Data.Set as S

import Data.Maybe
import qualified Data.Text as T
import Data.List (sort)
import Prelude hiding (mapM)
import Data.Traversable (mapM)
import Control.Monad.Except hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Control.Monad.State hiding (mapM)
import Utils.Time

import qualified Reporting.Errors.Def as Errors
import Reporting.Errors.Position
import Utils.Similarity
import Reporting.Logs
import qualified Program.Syntax as Syntax

import Optimizer.Def
import Optimizer.Env
import Typings.Env (HasCurrentScopeVars(currentScopeVars))

data Value = Constant (Syntax.Lit Position) | Dynamic
    deriving (Eq, Show)

data ConstPropagationEnv = ConstPropagationEnv {
    _cpVars :: M.Map String (Syntax.Ident Position, Value) --[(Syntax.Ident Position, Value)]
    , _cpCurrentScopeStart :: Maybe (Syntax.Stmt Position)
    , _cpScopes :: [S.Set String]
    , _cpCurrentScopeVars :: M.Map String (Syntax.Ident Position, Value)
    , _cpRenamedVars :: M.Map String String
    , _cpContextBlock :: Maybe (Syntax.Block Position)
    , _cpNextFreeGlobalBlockUID :: Int
    , _cpBlockIndex :: Int
}

makeLensesWith abbreviatedFields ''ConstPropagationEnv

initialState :: ConstPropagationEnv
initialState = ConstPropagationEnv {
    _cpVars = M.empty
    , _cpCurrentScopeStart = Nothing
    , _cpCurrentScopeVars = M.empty
    , _cpScopes = []
    , _cpRenamedVars = M.empty
    , _cpContextBlock = Nothing
    , _cpNextFreeGlobalBlockUID = 0
    , _cpBlockIndex = 0
}

buildNewRun :: Int -> ConstPropagationEnv -> ConstPropagationEnv -> ConstPropagationEnv
buildNewRun noShift prev new = prev & nextFreeGlobalBlockUID .~ (noShift + new^.nextFreeGlobalBlockUID) & blockIndex .~ (noShift + new^.blockIndex)

run :: Syntax.Program Position -> Optimizer ConstPropagationEnv (Syntax.Program Position)
run prog = do
    startTime <- liftPipelineOpt nanos
    --normalizeScope =<< transformBools =<< foldConst p
    (_, optimizedProg) <- findFixedPoint [foldConst, transformBools, normalizeScope] (0, startTime) prog
    return optimizedProg
    where
        findFixedPoint :: [(Syntax.Program Position -> Optimizer ConstPropagationEnv (Syntax.Program Position))] -> (Int, Int) -> (Syntax.Program Position) -> Optimizer ConstPropagationEnv (Int, Syntax.Program Position)
        findFixedPoint fns (callNo, startTime) prog = do
            liftPipelineOpt $ printLogInfoStr $ "ConstPropagation.run before:" ++ (printi 0 prog)
            prevState <- oState
            let prevRepr = printi 0 prog
            newProg' <- foldM (\oldProg fn -> oStateSet (buildNewRun 1000000 prevState) >> fn oldProg) prog fns
            oStateSet $ const prevState
            newProg <- foldConst newProg'
            liftPipelineOpt $ printLogInfoStr $ "ConstPropagation.run after:" ++ (printi 0 newProg)
            oStateSet $ const prevState
            let newRepr = printi 0 newProg
            currentTime <- liftPipelineOpt nanos
            timeElapsedMs <- return $ div (currentTime - startTime) 1000000
            liftPipelineOpt $ printLogInfoStr $ "Optimizing AST round " ++ (show $ callNo+1) ++ " (took " ++ (show timeElapsedMs) ++ " ms)" 
            if newRepr /= prevRepr && timeElapsedMs <= 4000 then do
                findFixedPoint fns (callNo+1, startTime) newProg
            else return (callNo, newProg)

checkNull :: Syntax.Expr Position -> Syntax.Expr Position -> Optimizer ConstPropagationEnv ()
checkNull (Syntax.Lit _ (Syntax.Null _)) originalExpr = failure (\(tcEnv, oEnv) -> Errors.ExpressionAlwaysNull tcEnv oEnv originalExpr) --throw ("Expression is always null", pos)
checkNull _ _ = return ()

checkNegative :: Syntax.Expr Position -> Syntax.Expr Position -> Optimizer ConstPropagationEnv ()
checkNegative (Syntax.Lit _ (Syntax.Int _ i)) originalExpr | i < 0 = failure (\(tcEnv, oEnv) -> Errors.IndexAlwaysNegative tcEnv oEnv originalExpr)
checkNegative _ _ = return ()

extractAssignedVars :: Syntax.Stmt Position -> [Syntax.Ident Position]
extractAssignedVars (Syntax.Assignment _ (Syntax.Var _ id) _) = [id]
extractAssignedVars (Syntax.BlockStmt _ (Syntax.Block _ stmts)) = walk stmts
  where
    walk ((Syntax.VarDecl _ ds):ss) = filter (\(Syntax.Ident _ n) -> not $ elem n (names ds)) (walk ss)
    walk (s:ss) = extractAssignedVars s ++ walk ss
    walk [] = []
    names ((_, Syntax.NoInit _ (Syntax.Ident _ n)):ds) = n : names ds
    names ((_, Syntax.Init _ (Syntax.Ident _ n) _):ds) = n : names ds
    names [] = []
extractAssignedVars (Syntax.While _ _ s) = extractAssignedVars s
extractAssignedVars (Syntax.IfElse _ _ sl sr) = extractAssignedVars sl ++ extractAssignedVars sr
extractAssignedVars _ = []

addVarEnv :: Syntax.Ident Position -> Value -> ConstPropagationEnv -> ConstPropagationEnv
addVarEnv name@(Syntax.Ident _ id) val env = env & scopes %~ (\scp -> (S.insert id $ head scp) : tail scp) & nextFreeGlobalBlockUID %~ (+1) & vars %~ M.insert id (name, val) & renamedVars %~ (M.insert id $ "_var_" ++ show (env^.nextFreeGlobalBlockUID) ++ "#" ++ extractOriginalId id)
    where
        extractOriginalId :: String -> String
        extractOriginalId name = T.unpack $ last $ T.splitOn (T.pack "#") (T.pack name)

addVar :: Syntax.Ident Position -> Value -> Optimizer ConstPropagationEnv ()
addVar name val = oStateSet (addVarEnv name val)

exprToValue :: Syntax.Expr Position -> Value
exprToValue (Syntax.Lit a l) = Constant l
exprToValue _ = Dynamic

isInCurrentScope :: String -> Optimizer ConstPropagationEnv Bool
isInCurrentScope name = do
    scp <- oStateGet (head . (^.scopes))
    return $ S.member name $ scp

withBlockContext :: (Syntax.Block Position) -> Optimizer ConstPropagationEnv a -> Optimizer ConstPropagationEnv a
withBlockContext block@(Syntax.Block _ _) m = do
    prevBlockContext <- oStateGet (\env -> env^.contextBlock)
    prevRenamedVars <- oStateGet (\env -> env^.renamedVars)
    withOState (\env -> env & scopes %~ tail & contextBlock .~ prevBlockContext & nextFreeGlobalBlockUID %~ (+1) & renamedVars .~ prevRenamedVars) . return =<< withOState (\env -> env & scopes %~ ((:) S.empty) & contextBlock .~ Just block & nextFreeGlobalBlockUID %~ (+1)) m

withSepratateScope :: (Syntax.Stmt Position) -> Optimizer ConstPropagationEnv a -> Optimizer ConstPropagationEnv a
withSepratateScope scopeStart m = do
    prevScopeStart <- oStateGet (\env -> env^.currentScopeStart)
    prevScopeVars <- oStateGet (\env -> env^.currentScopeVars)
    prevRenamedVars <- oStateGet (\env -> env^.renamedVars)
    prevVars <- oStateGet (\env -> env^.vars)
    withOState (\env -> env & scopes %~ tail & currentScopeStart .~ prevScopeStart & vars .~ prevVars & renamedVars .~ prevRenamedVars & currentScopeVars .~ prevScopeVars & scopes %~ ((:) S.empty) & nextFreeGlobalBlockUID %~ (+1)) . return =<< withOState (\env -> env & currentScopeStart .~ Just scopeStart & currentScopeVars .~ M.empty & nextFreeGlobalBlockUID %~ (+1)) m

withScopedVars :: [(Syntax.Ident Position, Value)] -> Optimizer ConstPropagationEnv a -> Optimizer ConstPropagationEnv a
withScopedVars varsList m = do
    prevVars <- oStateGet (\env -> env^.vars)
    scopeVarNames <- return $ S.fromList $ map (\(Syntax.Ident _ n, _) -> n) varsList
    withOState (\env -> env & scopes %~ tail & vars .~ prevVars & currentScopeStart .~ Nothing & currentScopeVars .~ M.empty & nextFreeGlobalBlockUID %~ (+1)) . return =<< withOState (\env -> foldl (flip $ uncurry addVarEnv) (env & currentScopeStart .~ Nothing & scopes %~ ((:) scopeVarNames) & currentScopeVars .~ M.empty & nextFreeGlobalBlockUID %~ (+1)) varsList) m

zero :: Syntax.Type Position -> Value
zero (Syntax.IntT p) = Constant (Syntax.Int p 0)
zero (Syntax.ByteT p) = Constant (Syntax.Byte p 0)
zero (Syntax.BoolT p) = Constant (Syntax.Bool p False)
zero ast = Constant (Syntax.Null $ Syntax.getPos ast)

zeroExpr :: Syntax.Type Position -> Syntax.Lit Position
zeroExpr t = let (Constant c) = zero t in c

class (Syntax.IsSyntax a Position) => ConstFoldable a where
    doFoldConst :: a Position -> Optimizer ConstPropagationEnv (a Position)
    doTransformBools :: a Position -> Optimizer ConstPropagationEnv (a Position)
    doNormalizeScope :: a Position -> Optimizer ConstPropagationEnv (a Position)

    normalizeScope :: a Position -> Optimizer ConstPropagationEnv (a Position)
    normalizeScope ast = do
        --liftPipelineOpt $ printLogInfoStr $ "normalize scope " ++ (show ast) 
        withStateT (optimizerQuit $ Syntax.getPos ast) . return =<< withStateT (optimizerEnter $ Syntax.getPos ast) (doNormalizeScope ast)

    transformBools :: a Position -> Optimizer ConstPropagationEnv (a Position)
    transformBools ast = do
        --liftPipelineOpt $ printLogInfoStr $ "transform bools " ++ (show ast) 
        withStateT (optimizerQuit $ Syntax.getPos ast) . return =<< withStateT (optimizerEnter $ Syntax.getPos ast) (doTransformBools ast)

    foldConst :: a Position -> Optimizer ConstPropagationEnv (a Position)
    foldConst ast = do
        --liftPipelineOpt $ printLogInfoStr $ "optimize " ++ (show ast) 
        withStateT (optimizerQuit $ Syntax.getPos ast) . return =<< withStateT (optimizerEnter $ Syntax.getPos ast) (doFoldConst ast)


instance ConstFoldable Syntax.Program where
    doFoldConst (Syntax.Program p defs) = do
        ndefs <- mapM foldConst defs
        return (Syntax.Program p ndefs)
    doTransformBools (Syntax.Program p defs) = do
        ndefs <- mapM transformBools defs
        return (Syntax.Program p ndefs)
    doNormalizeScope (Syntax.Program p defs) = do
        ndefs <- mapM normalizeScope defs
        return (Syntax.Program p ndefs)

instance ConstFoldable Syntax.ClassDecl where
    doFoldConst (Syntax.MethodDecl p t id args b) = do
        nb <- withScopedVars (map (\(Syntax.Arg _ _ id) -> (id, Dynamic)) args) (foldConst b)
        return (Syntax.MethodDecl p t id args nb)
    doFoldConst ast = return ast 

    doTransformBools (Syntax.MethodDecl p t id args b) = do
        nb <- withScopedVars (map (\(Syntax.Arg _ _ id) -> (id, Dynamic)) args) (transformBools b)
        return (Syntax.MethodDecl p t id args nb)
    doTransformBools ast = return ast 

    doNormalizeScope (Syntax.MethodDecl p t id args b) = do
        nb <- withScopedVars (map (\(Syntax.Arg _ _ id) -> (id, Dynamic)) args) (normalizeScope b)
        newArgs <- mapM (\(Syntax.Arg p t (Syntax.Ident idpos name)) -> oStateGet (\env -> let (Just n) = M.lookup name $ env^.renamedVars in (Syntax.Arg p t (Syntax.Ident idpos n)))) args
        return (Syntax.MethodDecl p t id newArgs nb)
    doNormalizeScope ast = return ast 

instance ConstFoldable Syntax.Definition where
    doFoldConst (Syntax.FunctionDef p t id args b) = do
        nb <- withScopedVars (map (\(Syntax.Arg _ _ id) -> (id, Dynamic)) args) (foldConst b)
        return (Syntax.FunctionDef p t id args nb)
    doFoldConst (Syntax.ClassDef p id par mems) = do
        nmems <- mapM foldConst mems
        return (Syntax.ClassDef p id par nmems)

    doTransformBools (Syntax.FunctionDef p t id args b) = do
        nb <- withScopedVars (map (\(Syntax.Arg _ _ id) -> (id, Dynamic)) args) (transformBools b)
        return (Syntax.FunctionDef p t id args nb)
    doTransformBools (Syntax.ClassDef p id par mems) = do
        nmems <- mapM transformBools mems
        return (Syntax.ClassDef p id par nmems)

    doNormalizeScope (Syntax.FunctionDef p t id args b) = do
        nb <- withScopedVars (map (\(Syntax.Arg _ _ id) -> (id, Dynamic)) args) (normalizeScope b)
        newArgs <- mapM (\(Syntax.Arg p t (Syntax.Ident idpos name)) -> oStateGet (\env -> let (Just n) = M.lookup name $ env^.renamedVars in (Syntax.Arg p t (Syntax.Ident idpos n)))) args
        return (Syntax.FunctionDef p t id newArgs nb)
    doNormalizeScope (Syntax.ClassDef p id par mems) = do
        nmems <- mapM normalizeScope mems
        return (Syntax.ClassDef p id par nmems)

instance ConstFoldable  Syntax.Block where
    doFoldConst block@(Syntax.Block p stmts) = do
        nstmts <- withBlockContext block $ mapM foldConst stmts
        return (Syntax.Block p nstmts)

    doTransformBools block@(Syntax.Block p stmts) = do
        nstmts <- withBlockContext block $ mapM transformBools stmts
        return (Syntax.Block p nstmts)

    doNormalizeScope block@(Syntax.Block p stmts) = do
        nstmts <- withBlockContext block $ mapM normalizeScopeStmt stmts
        return (Syntax.Block p $ concat nstmts)
        where
            normalizeScopeStmt :: Syntax.Stmt Position -> Optimizer ConstPropagationEnv [Syntax.Stmt Position]
            normalizeScopeStmt (Syntax.BlockStmt p b) = do
                (Syntax.Block _ stmts) <- normalizeScope b
                return stmts
            normalizeScopeStmt ast = do
                sub <- normalizeScope ast
                return [sub]

instance ConstFoldable Syntax.Stmt where
    doFoldConst e@(Syntax.Empty _) = return e
    doFoldConst stmt@(Syntax.BlockStmt p b) = (withSepratateScope stmt $ foldConst b) >>= \(nb) -> return $ Syntax.BlockStmt p nb
    doFoldConst (Syntax.VarDecl p decls) = do
        ndecls <- foldDecls decls
        return $ Syntax.VarDecl p ndecls
        where
            foldDecls (d@(t, Syntax.NoInit p id):ds) = do
                addVar id $ zero t
                nds <- foldDecls ds
                return ((t, Syntax.Init p id (Syntax.Lit p (zeroExpr t))):nds)
            foldDecls ((t, Syntax.Init p id e):ds) = do
                ne <- foldConst e
                addVar id $ exprToValue ne
                nds <- foldDecls ds
                return ((t, Syntax.Init p id ne):nds)
            foldDecls [] = return []
    doFoldConst (Syntax.Assignment p el er) = do
        ne <- foldConst er
        case el of
            (Syntax.Var _ id@(Syntax.Ident _ varName)) -> do
                env <- ask
                iif <- isInCurrentScope varName --oStateGet (\env -> isJust $ env^.currentScopeStart)
                if iif then addVar id $ exprToValue ne
                    else addVar id Dynamic
                return $ Syntax.Assignment p el ne
            stmt@(Syntax.ArrAccess pp earr eidx m) -> do
                nearr <- foldConst earr
                neidx <- foldConst eidx
                checkNull nearr stmt
                checkNegative neidx stmt
                return $ Syntax.Assignment p (Syntax.ArrAccess pp nearr neidx m) ne
            stmt@(Syntax.Member pp eobj i mt) -> do
                neobj <- foldConst eobj
                checkNull neobj stmt
                return $ Syntax.Assignment p (Syntax.Member pp neobj i mt) ne
    doFoldConst (Syntax.ReturnValue p e) = do
        ne <- foldConst e
        return $ Syntax.ReturnValue p ne
    doFoldConst s@(Syntax.ReturnVoid _) = return s
    doFoldConst stmt@(Syntax.IfElse p econd strue sfalse) = do
        nec <- foldConst econd
        case nec of
            Syntax.Lit _ (Syntax.Bool _ b) -> if b then doFoldConst strue
                                else doFoldConst sfalse
            _ -> do
                nst <- withSepratateScope stmt (doFoldConst strue)
                nsf <- withSepratateScope stmt (doFoldConst sfalse)
                return $ Syntax.IfElse p nec nst nsf
    doFoldConst stmt@(Syntax.While p ec s) = do
        nec <- foldConst ec
        case nec of
            Syntax.Lit _ (Syntax.Bool _ False) -> return $ Syntax.Empty p
            _ -> do
                mapM_ (\n -> addVar n Dynamic) $ extractAssignedVars s
                nec <- foldConst ec
                ns <- withSepratateScope stmt (doFoldConst s)
                return $ Syntax.While p nec ns
    doFoldConst (Syntax.ExprStmt p e) = foldConst e >>= \ne -> return $ Syntax.ExprStmt p ne

    doTransformBools (Syntax.BlockStmt p b) = do
        nb <- transformBools b
        return $ Syntax.BlockStmt p nb
    doTransformBools (Syntax.Assignment p el er) | Syntax.isCond er =
        return $ Syntax.IfElse p er
                    (Syntax.Assignment p el (Syntax.Lit p (Syntax.Bool p True)))
                    (Syntax.Assignment p el (Syntax.Lit p (Syntax.Bool p False)))
    doTransformBools (Syntax.ReturnValue p e) | Syntax.isCond e =
        return $ Syntax.IfElse p e
                    (Syntax.ReturnValue p (Syntax.Lit p (Syntax.Bool p True)))
                    (Syntax.ReturnValue p (Syntax.Lit p (Syntax.Bool p False)))
    doTransformBools (Syntax.IfElse p e s1 s2) = do
        s1n <- transformBools s1
        s2n <- transformBools s2
        return $ Syntax.IfElse p e s1n s2n
    doTransformBools (Syntax.While p e s) = do
        sn <- transformBools s
        return $ Syntax.While p e sn
    doTransformBools s = return s

    -- chyba tu?
    doNormalizeScope stmt@(Syntax.BlockStmt p b) = (withSepratateScope stmt $ normalizeScope b) >>= \(nb) -> return $ Syntax.BlockStmt p nb
    doNormalizeScope (Syntax.VarDecl p decls) = do
        ndecls <- normalizeScopeInDecls decls
        return $ Syntax.VarDecl p ndecls
        where
            normalizeScopeInDecls (d@(t, Syntax.NoInit p id@(Syntax.Ident idpos name)):ds) = do
                addVar id $ zero t
                newName <- oStateGet (\env -> let (Just n) = M.lookup name $ env^.renamedVars in n)
                nds <- normalizeScopeInDecls ds
                return ((t, Syntax.NoInit p (Syntax.Ident idpos newName)):nds)
            normalizeScopeInDecls ((t, Syntax.Init p id@(Syntax.Ident idpos name) e):ds) = do
                ne <- normalizeScope e
                addVar id $ exprToValue ne
                newName <- oStateGet (\env -> let (Just n) = M.lookup name $ env^.renamedVars in n)
                nds <- normalizeScopeInDecls ds
                return ((t, Syntax.Init p (Syntax.Ident idpos newName) ne):nds)
            normalizeScopeInDecls [] = return []
    doNormalizeScope (Syntax.Assignment p el er) = do
        nel <- normalizeScope el
        ner <- normalizeScope er
        return (Syntax.Assignment p nel ner)
    doNormalizeScope (Syntax.ReturnValue p e) = do
        ne <- normalizeScope e
        return (Syntax.ReturnValue p ne)
    doNormalizeScope (Syntax.IfElse p ec s1 s2) = do
        nec <- normalizeScope ec
        ns1 <- normalizeScope s1
        ns2 <- normalizeScope s2
        return (Syntax.IfElse p nec ns1 ns2)
    doNormalizeScope (Syntax.While p ec s) = do
        nec <- normalizeScope ec
        ns <- normalizeScope s
        return (Syntax.While p nec ns)
    doNormalizeScope (Syntax.ExprStmt p e) = do
        ne <- normalizeScope e
        return (Syntax.ExprStmt p ne)
    doNormalizeScope s = return s

-- ezero t = let (Constant c) = zero t in c

-- foldConstants :: Program Position -> InnerMonad (Program Position)
-- foldConstants (Program p defs) = do
--     ndefs <- runReaderT (evalStateT (mapM foldD defs) 0) []
--     return (Program p ndefs)


-- foldMem (MethodDecl p t id args b) = do
--     (nb,_) <- local (fromArgs args) (foldB b)
--     return (MethodDecl p t id args nb)
-- foldMem m = return m

-- throw = lift . lift . throwError

-- fromArgs args _ = map (\(Arg _ _ id) -> (id, Dynamic)) args

-- foldB :: Block Position -> OuterMonad (Block Position, Environment -> Environment)
-- foldB (Block p stmts) = do
--     (addblock, removeblock) <- newBlock
--     (nstmts, f) <- local addblock (foldStmts stmts)
--     return (Block p nstmts, removeblock . f . addblock)
--   where
--     foldStmts :: [Stmt Position] -> OuterMonad ([Stmt Position], Environment -> Environment)
--     foldStmts [] = return ([], id)
--     foldStmts (s:ss) = do
--         {-debug-
--         env<-ask
--         trace ("\n"++show env) return ()
--         -end debug-}
--         (ns, f) <- doFoldConst s
--         (nss, ff) <- local f (foldStmts ss)
--         return (ns:nss, ff . f)

-- name str = Ident BuiltIn str

-- newBlock :: OuterMonad (Environment -> Environment, Environment -> Environment)
-- newBlock = do
--     i <- get
--     put (i+1)
--     let blockName = "$block"++show i
--     return (envAdd (name blockName) Marker, removeBlock blockName)

-- envAdd id v = (:) (id,v)
-- envExp id e = envAdd id $ valExp e

-- valExp (Lit a l) = Constant l
-- valExp _ = Dynamic

-- envUpdate ui@(Ident _ m) v ((i@(Ident p n), vv):is) =
--     if n == m then (i, v) : is
--     else (i,vv): envUpdate ui v is
-- envUpdateExp ui e is = envUpdate ui (valExp e) is

-- removeBlock n es = 
--     case indexOfBlock n es 0 of
--         Nothing -> es
--         Just i -> drop i es
--     where
--         indexOfBlock m ((Ident _ n, _):es) i =
--             if n == m then Just (i+1)
--             else indexOfBlock m es (i+1)
--         indexOfBlock _ [] _ = Nothing



-- outOfIf id ((Ident _ "$if", _):es) = do
--     m <- find id es
--     case m of
--         Just _ -> return True
--         _ -> return False
-- outOfIf id (_:es) = outOfIf id es
-- outOfIf _ [] = return False
    
-- find i@(Ident _ m) ((Ident _ n, v):es) = 
--     if m == n then return (Just v)
--     else find i es
-- find _ [] = return Nothing

-- doFoldConst :: Stmt Position -> OuterMonad (Stmt Position, Environment -> Environment)
-- doFoldConst e@(Empty _) = return (e, id)
-- doFoldConst (BlockStmt p b) = foldB b >>= \(nb, f) -> return (BlockStmt p nb, f)
-- doFoldConst (VarDecl p decls) = do
--     (ndecls, f) <- foldDecls decls
--     return (VarDecl p ndecls, f)
--     where
--         foldDecls (d@(t, NoInit p id):ds) = do
--             (nds, f) <- local (envAdd id $ zero t) (foldDecls ds)
--             return ((t, Init p id (Lit p (ezero t))):nds, f . (envAdd id $ zero t))
--         foldDecls ((t, Init p id e):ds) = do
--             ne <- foldConst e
--             (nds, f) <- local (envExp id ne) (foldDecls ds)
--             return ((t, Init p id ne):nds, f . envExp id ne)
--         foldDecls [] = return ([], id)
-- doFoldConst (Assignment p el er) = do
--     ne <- foldConst er
--     case el of
--         (Var _ id) -> do
--             env <- ask
--             iif <- outOfIf id env
--             let f = if not iif then envUpdateExp id ne
--                     else envUpdate id Dynamic
--             return (Assignment p el ne, f)
--         (ArrAccess pp earr eidx m) -> do
--             nearr <- foldConst earr
--             neidx <- foldConst eidx
--             checkNull nearr pp
--             checkNegative neidx pp
--             return (Assignment p (ArrAccess pp nearr neidx m) ne, id)
--         (Member pp eobj i mt) -> do
--             neobj <- foldConst eobj
--             checkNull neobj pp
--             return (Assignment p (Member pp neobj i mt) ne, id)
-- doFoldConst (ReturnValue p e) = do
--     ne <- foldConst e
--     return (ReturnValue p ne, id)
-- doFoldConst s@(ReturnVoid _) = return (s, id)
-- doFoldConst (IfElse p econd strue sfalse) = do
--     nec <- foldConst econd
--     case nec of
--         Lit _ (Bool _ b) -> if b then doFoldConst strue
--                             else doFoldConst sfalse
--         _ -> do
--             let f = envAdd (name "$if") Marker
--             (nst, f1) <- local f (doFoldConst strue)
--             (nsf, f2) <- local f (doFoldConst sfalse)
--             return (IfElse p nec nst nsf, f2 . f1)
-- doFoldConst (While p ec s) = do
--     nec <- foldConst ec
--     case nec of
--         Lit _ (Bool _ False) -> return (Empty p, id)
--         _ -> do
--             let f = envAdd (name "$if") Marker
--                 assigned = extractAssignedVars s
--                 dyns = foldr (\n a -> a . envUpdate n Dynamic) id assigned
--             nec <- local dyns (foldConst ec)
--             (ns, fs) <- local (dyns . f) (doFoldConst s)
--             return (While p nec ns, fs . dyns)
-- doFoldConst (ExprStmt p e) = foldConst e >>= \ne -> return (ExprStmt p ne, id)

-- extractAssignedVars :: Stmt Position -> [Ident Position]
-- extractAssignedVars (Assignment _ (Var _ id) _) = [id]
-- extractAssignedVars (BlockStmt _ (Block _ stmts)) = walk stmts
--   where
--     walk ((VarDecl _ ds):ss) = filter (\(Ident _ n) -> not $ elem n (names ds)) (walk ss)
--     walk (s:ss) = extractAssignedVars s ++ walk ss
--     walk [] = []
--     names ((_, NoInit _ (Ident _ n)):ds) = n : names ds
--     names ((_, Init _ (Ident _ n) _):ds) = n : names ds
--     names [] = []
-- extractAssignedVars (While _ _ s) = extractAssignedVars s
-- extractAssignedVars (IfElse _ _ sl sr) = extractAssignedVars sl ++ extractAssignedVars sr
-- extractAssignedVars _ = []

boundB :: (Ord a, Num a) => a -> Bool
boundB x = x >= 0 && x < 256
boundI :: (Num a, Ord a) => a -> Bool
boundI x = x >= -(2^31) && x < 2^31

sameExp :: (Eq (f ()), Functor f) => f a1 -> f a2 -> Bool
sameExp e1 e2 = fmap nill e1 == fmap nill e2

nill :: p -> ()
nill _ = ()

type LinearizedExprTree = [Syntax.Expr Position]
linearize (Syntax.BinaryOp p op el er) =
    let left = case el of
                Syntax.BinaryOp _ op2 _ _ ->
                    if fmap nill op == fmap nill op2 then
                        linearize el
                    else [el]
                _ -> [el]
        right = case er of
                 Syntax.BinaryOp _ op2 _ _ ->
                    if fmap nill op == fmap nill op2 then
                        linearize er
                    else [er]
                 _ -> [er]
    in left ++ right

checkForNullComparison :: Syntax.Expr Position -> Optimizer ConstPropagationEnv (Syntax.Expr Position)
checkForNullComparison (Syntax.App p (Syntax.Member pp el (Syntax.Ident _ eq) mt) [l@(Syntax.Lit ppp (Syntax.Null pppp))]) | eq == "equals" =
    return (Syntax.BinaryOp p (Syntax.Equ pp) el l)
checkForNullComparison e = checkStringConcat e

checkStringConcat ::Syntax.Expr Position -> Optimizer ConstPropagationEnv (Syntax.Expr Position)
checkStringConcat (Syntax.App p (Syntax.Member pp (Syntax.Lit _ (Syntax.String _ s)) (Syntax.Ident _ c) mt) [Syntax.Lit _ (Syntax.String _ s2)]) | c == "concat" =
    return (Syntax.Lit pp (Syntax.String pp (s ++ s2)))
checkStringConcat e = return e


instance ConstFoldable Syntax.Expr where

    doTransformBools ast = return ast

    doFoldConst l@(Syntax.Lit _ _) = return l
    doFoldConst (Syntax.App p (Syntax.Member _ (Syntax.Lit _ (Syntax.Null _)) (Syntax.Ident _ "equals") mt) [(Syntax.Lit _ (Syntax.Null _))]) = return $ Syntax.Lit p (Syntax.Bool p True)
    doFoldConst (Syntax.App p (Syntax.Member p4 (Syntax.Lit p2 (Syntax.Null p3)) (Syntax.Ident p5 "equals") mt) [es]) =
        foldConst (Syntax.App p (Syntax.Member p4 es (Syntax.Ident p5 "equals") mt) [Syntax.Lit p2 (Syntax.Null p3)])
    doFoldConst expr@(Syntax.App p el es) = do
        nel <- foldConst el
        checkNull nel expr
        nes <- mapM foldConst es
        checkForNullComparison (Syntax.App p nel nes)
    doFoldConst expr@(Syntax.Member p el id mt) = do
        nel <- foldConst el
        checkNull nel expr
        return (Syntax.Member p nel id mt)
    doFoldConst (Syntax.NewObj p t me) = do
        nme <- mapM foldConst me
        return (Syntax.NewObj p t nme)
    doFoldConst expr@(Syntax.ArrAccess p el er m) = do
        nel <- foldConst el
        checkNull nel expr
        ner <- foldConst er
        checkNegative ner expr
        return (Syntax.ArrAccess p nel ner m)
    doFoldConst (Syntax.Cast p t e) = do
        ne <- foldConst e
        case (t, ne) of
            (Syntax.IntT _, Syntax.Lit p2 (Syntax.Byte _ b)) -> return (Syntax.Lit p2 (Syntax.Int p2 b))
            (Syntax.ByteT _, Syntax.Lit p2 (Syntax.Int _ b)) -> 
                if b < 256 && b >= 0 then return (Syntax.Lit p2 (Syntax.Byte p2 b))
                else return (Syntax.Cast p t ne)
            (Syntax.ClassT _ _, Syntax.Lit p2 (Syntax.Null _)) -> return (Syntax.Lit p2 (Syntax.Null p2))
            _ -> return (Syntax.Cast p t ne)
    doFoldConst (Syntax.Var p id@(Syntax.Ident _ n)) = do
        env <- ask
        m <- oStateGet (\env -> snd <$> (M.lookup n $ env^.vars))
        case m of
            Just Dynamic -> return (Syntax.Var p id)
            Just (Constant l) -> return (Syntax.Lit p l)
            Nothing -> return (Syntax.Var p id)
    doFoldConst (Syntax.UnaryOp p op e) = do
        ne <- foldConst e
        case op of
            Syntax.Neg _ -> case ne of
                        Syntax.Lit _ (Syntax.Int _ i) -> return (Syntax.Lit p (Syntax.Int p (-i)))
                        Syntax.Lit _ (Syntax.Byte _ i) -> return (Syntax.Lit p (Syntax.Byte p (-i)))
                        _ -> return (Syntax.UnaryOp p op ne)
            Syntax.Not _ -> case ne of
                        Syntax.Lit _ (Syntax.Bool _ b) -> return (Syntax.Lit p (Syntax.Bool p (not b)))
                        _ -> return (Syntax.UnaryOp p op ne)
    doFoldConst parent@(Syntax.BinaryOp p op el er) = do 
        nel <- foldConst el
        ner <- foldConst er
        let ne = (Syntax.BinaryOp p op nel ner)
            sp = specialExp ne
        if fmap nill sp /= fmap nill ne then foldConst sp
        else case op of
            Syntax.Equ _ -> if sameExp nel ner then return (Syntax.Lit p (Syntax.Bool p True))
                    else return (Syntax.BinaryOp p op nel ner)
            Syntax.Neq _ -> checkConst (/= EQ) nel ner (Syntax.BinaryOp p op nel ner)
            Syntax.Gt _ -> checkConst (== GT) nel ner (Syntax.BinaryOp p op nel ner)
            Syntax.Ge _ -> checkConst (\c -> c == GT || c == EQ) nel ner (Syntax.BinaryOp p op nel ner)
            Syntax.Lt _ -> checkConst (== LT) nel ner (Syntax.BinaryOp p op nel ner)
            Syntax.Le _ -> checkConst (\c -> c == LT || c == EQ) nel ner (Syntax.BinaryOp p op nel ner)

            _ -> do
                let lin = linearize (Syntax.BinaryOp p op nel ner)
                    nop = fmap nill op
                if nop == Syntax.Div () || nop == Syntax.Mod () || nop == Syntax.Sub () || nop == Syntax.And () || nop == Syntax.Or () then do
                    fslin <- foldconsts parent op lin
                    return $ foldl1 (Syntax.BinaryOp p op) fslin
                else do
                    fslin <- foldconsts parent op $ sort lin
                    return $ foldl1 (Syntax.BinaryOp p op) fslin
        where
            true p = (Syntax.Lit p (Syntax.Bool p True))
            false p = (Syntax.Lit p (Syntax.Bool p False))
            checkConst :: (Ordering -> Bool) -> Syntax.Expr Position -> Syntax.Expr Position -> Syntax.Expr Position -> Optimizer ConstPropagationEnv (Syntax.Expr Position)
            checkConst f (Syntax.Lit _ x) (Syntax.Lit _ y) r =
                case (x,y) of
                    (Syntax.Int p i, Syntax.Int _ j) -> if f $ compare i j then return (true p)
                                        else return (false p)
                    (Syntax.Byte p i, Syntax.Byte _ j) -> if f $ compare i j then return (true p)
                                            else return (false p)
                    (Syntax.Bool p i, Syntax.Bool _ j) -> if f $ compare i j then return (true p)
                                        else return (false p)
                    (Syntax.String p i, Syntax.String _ j) -> if f $ compare i j then return (true p)
                                        else return (false p)
                    (l1, l2) -> failure (\(tcEnv, oEnv) -> Errors.InternalOptimizerFailure tcEnv oEnv "checkConst" $ Errors.IOPTECheckConstUnexpectedLiterals l1 l2)
            checkConst _ _ _ (Syntax.BinaryOp p op l@(Syntax.Lit _ _) e) = return (Syntax.BinaryOp p (reverseSide op) e l)
                    where
                        reverseSide (Syntax.Lt p) = (Syntax.Gt p)
                        reverseSide (Syntax.Le p) = (Syntax.Ge p)
                        reverseSide (Syntax.Gt p) = (Syntax.Lt p)
                        reverseSide (Syntax.Ge p) = (Syntax.Le p)
                        reverseSide op = op
            checkConst _ _ _ r = return r
            foldconsts :: Syntax.Expr Position -> Syntax.BinOp Position -> [Syntax.Expr Position] -> Optimizer ConstPropagationEnv [Syntax.Expr Position]
            foldconsts parent op@(Syntax.Add _) ((Syntax.Lit p (Syntax.String _ i)):(Syntax.Lit _ (Syntax.String _ j)):xs) = foldconsts parent op $ (Syntax.Lit p (Syntax.String p (i ++ j))):xs
            foldconsts parent op@(Syntax.Add _) ((Syntax.Lit p (Syntax.Int _ i)):(Syntax.Lit _ (Syntax.Int _ j)):xs) | boundI (i+j) = foldconsts parent op $ (Syntax.Lit p (Syntax.Int p (i+j))):xs
            foldconsts parent op@(Syntax.Sub _) ((Syntax.Lit p (Syntax.Int _ i)):(Syntax.Lit _ (Syntax.Int _ j)):xs) | boundI (i-j) = foldconsts parent op $ (Syntax.Lit p (Syntax.Int p (i-j))):xs
            foldconsts parent op@(Syntax.Mul _) ((Syntax.Lit p (Syntax.Int _ i)):(Syntax.Lit _ (Syntax.Int _ j)):xs) | boundI (i*j) = foldconsts parent op $ (Syntax.Lit p (Syntax.Int p (i*j))):xs
            foldconsts parent op@(Syntax.Div _) ((Syntax.Lit p (Syntax.Int _ i)):(Syntax.Lit _ (Syntax.Int _ j)):xs) | j == 0 = failure (\(tcEnv, oEnv) -> Errors.DivisionByZero tcEnv oEnv parent op) 
            foldconsts parent op@(Syntax.Div _) ((Syntax.Lit p (Syntax.Int _ i)):(Syntax.Lit _ (Syntax.Int _ j)):xs) | boundI (i `div` j) = foldconsts parent op $ (Syntax.Lit p (Syntax.Int p (i `div` j))):xs
            foldconsts parent op@(Syntax.Add _) ((Syntax.Lit p (Syntax.Byte _ i)):(Syntax.Lit _ (Syntax.Byte _ j)):xs) | boundB (i+j) = foldconsts parent op $ (Syntax.Lit p (Syntax.Byte p (i+j))):xs
            foldconsts parent op@(Syntax.Sub _) ((Syntax.Lit p (Syntax.Byte _ i)):(Syntax.Lit _ (Syntax.Byte _ j)):xs) | boundB (i-j) = foldconsts parent op $ (Syntax.Lit p (Syntax.Byte p (i-j))):xs
            foldconsts parent op@(Syntax.Mul _) ((Syntax.Lit p (Syntax.Byte _ i)):(Syntax.Lit _ (Syntax.Byte _ j)):xs) | boundB (i*j) = foldconsts parent op $ (Syntax.Lit p (Syntax.Byte p (i*j))):xs
            foldconsts parent op@(Syntax.Mod _) ((Syntax.Lit p (Syntax.Int _ i)):(Syntax.Lit _ (Syntax.Int _ j)):xs) | j == 0 = failure (\(tcEnv, oEnv) -> Errors.ModuloByZero tcEnv oEnv parent op) 
            foldconsts parent op@(Syntax.Mod _) ((Syntax.Lit p (Syntax.Int _ i)):(Syntax.Lit _ (Syntax.Int _ j)):xs) | boundI (i `mod` j) = foldconsts parent op $ (Syntax.Lit p (Syntax.Int p (i `mod` j))):xs
            foldconsts parent op@(Syntax.And _) ((Syntax.Lit p (Syntax.Bool _ i)):(Syntax.Lit _ (Syntax.Bool _ j)):xs) = foldconsts parent op $ (Syntax.Lit p (Syntax.Bool p (i && j))):xs
            foldconsts parent (Syntax.And _) (f@(Syntax.Lit _ (Syntax.Bool _ True)):[]) = return [f]
            foldconsts parent op@(Syntax.And _) ((Syntax.Lit _ (Syntax.Bool _ True)):xs) = foldconsts parent op xs
            foldconsts parent (Syntax.And _) (f@(Syntax.Lit _ (Syntax.Bool _ False)):xs) = return [f]
            foldconsts parent op@(Syntax.Or _) ((Syntax.Lit p (Syntax.Bool _ i)):(Syntax.Lit _ (Syntax.Bool _ j)):xs) = foldconsts parent op $ (Syntax.Lit p (Syntax.Bool p (i || j))):xs
            foldconsts parent (Syntax.Or _) (f@(Syntax.Lit _ (Syntax.Bool _ True)):xs) = return [f]
            foldconsts parent (Syntax.Or _) (f@(Syntax.Lit _ (Syntax.Bool _ False)):[]) = return [f]
            foldconsts parent op@(Syntax.Or _) ((Syntax.Lit _ (Syntax.Bool _ False)):xs) = foldconsts parent op xs
            foldconsts parent op (x:xs) = do
                xxs <- foldconsts parent op xs
                return $ x:xxs
            foldconsts _ _ [] = return []
            specialExp :: Syntax.Expr Position -> Syntax.Expr Position
            specialExp (Syntax.BinaryOp p (Syntax.Div pop) (Syntax.BinaryOp _ (Syntax.Div _) el er) e) = (Syntax.BinaryOp p (Syntax.Div pop) el (Syntax.BinaryOp p (Syntax.Mul p) er e))
            specialExp e = e

    doNormalizeScope e@(Syntax.Var p (Syntax.Ident pp n)) = do
        newName <- oStateGet (\env -> M.lookup n $ env^.renamedVars)
        case newName of
            Nothing -> return (Syntax.Var p (Syntax.Ident pp n))
            Just m -> return (Syntax.Var p (Syntax.Ident pp m))
    doNormalizeScope (Syntax.App p e es) = do
        ne <- normalizeScope e
        nes <- mapM normalizeScope es
        return (Syntax.App p ne nes)
    doNormalizeScope (Syntax.UnaryOp p op e) = do
        ne <- normalizeScope e
        return (Syntax.UnaryOp p op ne)
    doNormalizeScope (Syntax.BinaryOp p op e1 e2) = do
        ne1 <- normalizeScope e1
        ne2 <- normalizeScope e2
        return (Syntax.BinaryOp p op ne1 ne2)
    doNormalizeScope (Syntax.Member p e id mt) = do
        ne <- normalizeScope e
        return (Syntax.Member p ne id mt)
    doNormalizeScope (Syntax.NewObj p t (Just e)) = do
        ne <- normalizeScope e
        return (Syntax.NewObj p t (Just ne))
    doNormalizeScope (Syntax.ArrAccess p el er mt) = do
        nel <- normalizeScope el
        ner <- normalizeScope er
        return (Syntax.ArrAccess p nel ner mt)
    doNormalizeScope (Syntax.Cast p t e) = do
        ne <- normalizeScope e
        return (Syntax.Cast p t ne)
    doNormalizeScope e = return e


-- boundB x = x >= 0 && x < 256
-- boundI x = x >= -(2^31) && x < 2^31

-- sameExp e1 e2 = fmap nill e1 == fmap nill e2

-- nill _ = ()

-- type LinearizedExprTree = [Expr Position]
-- linearize (BinaryOp p op el er) =
--     let left = case el of
--                 BinaryOp _ op2 _ _ ->
--                     if fmap nill op == fmap nill op2 then
--                         linearize el
--                     else [el]
--                 _ -> [el]
--         right = case er of
--                  BinaryOp _ op2 _ _ ->
--                     if fmap nill op == fmap nill op2 then
--                         linearize er
--                     else [er]
--                  _ -> [er]
--     in left ++ right


-- checkNull :: Expr Position -> Position -> OuterMonad ()
-- checkNull (Lit _ (Null _)) pos = throw ("Expression is always null", pos)
-- checkNull _ _ = return ()

-- checkNegative :: Expr Position -> Position -> OuterMonad ()
-- checkNegative (Lit _ (Int _ i)) pos | i < 0 = throw ("Index is always negative", pos)
-- checkNegative _ _ = return ()

-- checkForNullComparison :: Expr Position -> OuterMonad (Expr Position)
-- checkForNullComparison (App p (Member pp el (Ident _ eq) mt) [l@(Lit ppp (Null pppp))]) | eq == "equals" =
--     return (BinaryOp p (Equ pp) el l)
-- checkForNullComparison e = checkStringConcat e

-- checkStringConcat :: Expr Position -> OuterMonad (Expr Position)
-- checkStringConcat (App p (Member pp (Lit _ (String _ s)) (Ident _ c) mt) [Lit _ (String _ s2)]) | c == "concat" =
--     return (Lit pp (String pp (s ++ s2)))
-- checkStringConcat e = return e
