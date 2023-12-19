module Typings.TypeChecking where

import qualified Program.Syntax as Syntax
import Typings.Def
import Typings.Types as Type
import Reporting.Errors.Position

import Control.Monad.State hiding (void)

import Typings.InheritanceHierarchy
import Typings.Env

import qualified Reporting.Errors.Def as Errors

class TypeCheckable a where
    checkTypes :: a Position -> TypeChecker (a Position)

withVar :: Type.Name -> Type.Type -> TypeChecker x -> TypeChecker x
withVar name t = withStateT (addVar name t)

canBeCastUp :: Syntax.Type Position -> Syntax.Type Position -> TypeChecker Bool
canBeCastUp tFrom tTo = do
    cls <- classes
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
        ((Syntax.ClassT _ (Syntax.Ident _ idSon)), (Syntax.ClassT _ (Syntax.Ident _ idPar))) -> if idSon == idPar then return True
                                          else return $ isParent cls idSon idPar
        (Syntax.ArrayT _ t1, Syntax.ArrayT _ t2) -> equivalentType t1 t2
        (Syntax.FunT _ t1 ts1, Syntax.FunT _ t2 ts2) -> do
            t <- canBeCastUp t1 t2
            let lcheck = length ts1 == length ts2
            cs <- mapM (\(t1, t2) -> canBeCastUp t1 t2) (zip ts1 ts2)
            return (all id (lcheck:t:cs))
        (Syntax.InfferedT _, _) -> return True -- only when checking Expr.App
        (Syntax.StringT _, Syntax.InfferedT _) -> return True -- only when casting null
        (Syntax.ClassT _ _, Syntax.InfferedT _) -> return True -- only when casting null
        (Syntax.ArrayT _ _, Syntax.InfferedT _) -> return True -- only when casting null
        _ -> return False

canBeCastDown :: Syntax.Type Position -> Syntax.Type Position -> TypeChecker Bool
canBeCastDown tFrom tTo = do
    cls <- classes
    case (tFrom, tTo) of
        (Syntax.IntT _, Syntax.ByteT _) -> return True
        (Syntax.ByteT _, Syntax.IntT _) -> return True
        (Syntax.ClassT _ (Syntax.Ident _ idFrom), Syntax.ClassT _ (Syntax.Ident _ idTo)) -> do
            b1 <- return $ isParent cls idTo idFrom
            b2 <- return $ isParent cls idFrom idTo
            return (b1 || b2)
        (Syntax.ClassT _ (Syntax.Ident _ "Object"), Syntax.ArrayT _ _) -> return True
        _ -> canBeCastUp tTo tFrom

checkTypeExists :: Type.AllowVoid -> Syntax.Type Position -> TypeChecker ()
checkTypeExists _ (Syntax.ClassT _ id@(Syntax.Ident pos n)) =
    maybe (failure $ Errors.UnknownFailure $ "Undeclared type "++n) (\_ -> return ()) =<< findClass n
        -- Just x -> return ()
        -- _ -> throw ("Undeclared type "++n, pos)
checkTypeExists _ (Syntax.ArrayT _ t) = checkTypeExists NoVoid t
checkTypeExists Type.NoVoid (Syntax.VoidT pos) = failure $ Errors.UnknownFailure "Illegal use of type void"
checkTypeExists _ _ = return ()

equivalentType :: Syntax.Type Position -> Syntax.Type Position -> TypeChecker Bool
equivalentType t1 t2 = do
    a <- canBeCastUp t1 t2
    b <- canBeCastUp t2 t1
    return (a && b)

instance TypeCheckable Syntax.Program where
    checkTypes (Syntax.Program pos defs) = mapM checkTypes defs >>= return . Syntax.Program pos

instance TypeCheckable Syntax.Definition where
    checkTypes (Syntax.FunctionDef pos tret id args b) = do
        checkTypeExists Type.AllowVoid tret
        mapM (lift . typeFromArg) args >>= mapM_ (checkTypeExists NoVoid)
        checkArgsRedeclaration args
        checkedBody <- local (funEnv tret args) (checkTypes b)
        return $ Syntax.FunctionDef pos tret id args checkedBody
    checkTypes (Syntax.ClassDef pos id parent decls) = do
        checkTypeExists Type.NoVoid (ClassT pos pid)
        checkedDecls <- local (classEnv pos id) (mapM checkTypes decls)
        return $ Syntax.ClassDef pos id (Just pid) checkedDecls
        where
            classEnv pos id (cls, funs, env) = (cls, funs, (name "$class", Syntax.ClassT pos id) : (Syntax.Ident pos "this", Syntax.ClassT pos id) : env)
            pid = case parent of
                    Just x -> x
                    Nothing -> let (Syntax.Ident (Position f l c) s) = id in Syntax.Ident (Position f l (c+length s)) "Object"
            pos = let (Syntax.Ident p _) = pid in p

instance TypeCheckable Syntax.ClassDecl where
    checkTypes f@(Syntax.FieldDecl pos t id) = do
        checkTypeExists Type.NoVoid t
        return f
    checkTypes (Syntax.MethodDecl pos tret id args b) = do
        checkTypeExists AllowVoid tret
        mapM (lift . typeFromArg) args >>= mapM_ (checkTypeExists NoVoid)
        checkArgsRedeclaration args
        checkedBody <- local (funEnv tret args) (checkTypes b)
        return $ Syntax.MethodDecl pos tret id args checkedBody

instance TypeCheckable Syntax.Block where
    checkTypes (Syntax.Block pos stmts) = do
        newStmts <- local addBlock (checkTypestmts stmts)
        return $ Syntax.Block pos newStmts
        where
            checkTypestmts (stmt:stmts) = do
                (nstmt, f) <- checkTypes stmt
                nstmts <- local f (checkTypestmts stmts)
                return $ nstmt : nstmts
            checkTypestmts [] = return []
            addBlock (cls, funs, env) = (cls, funs, (name "$block", void):env)

-- funEnv ret args (classes, functions, env) = (classes, functions, newenv)
--     where
--         newenv = map fromArg args ++ (name "$ret", ret) : env
--         fromArg (Arg pos t id) = (id, t)

-- checkArgsRedeclaration :: [Arg Position] -> OuterMonad ()
-- checkArgsRedeclaration args = do
--     let argNames = map (\(Arg _ _ (Ident p n))->(n,p)) args
--         argNameCheck = duplicates argNames
--     case argNameCheck of
--         [] -> return ()
--         (n,p):_ -> throw ("Redeclaration of argument of name "++n, p)

-- addVar id t (cls,funs,env) = (cls,funs,(id,t):env)

--checkTypes :: Stmt Position -> OuterMonad (Stmt Position, Environment -> Environment)
instance TypeCheckable Syntax.Stmt where
    checkTypes (Syntax.Empty pos) = return (Syntax.Empty pos, id)
    checkTypes (Syntax.BlockStmt pos b) = checkTypes b >>= \b -> return (Syntax.BlockStmt pos b, id)
    checkTypes (Syntax.VarDecl pos decls) = do
        (ndecls, f) <- checkDecls decls
        return $ (Syntax.VarDecl pos ndecls, f)
        where
            checkDecls :: [(Syntax.Type Position, Syntax.DeclItem Position)] -> TypeChecker ([(Syntax.Type Position, Syntax.DeclItem Position)], TypeCheckerEnv -> TypeCheckerEnv) 
            checkDecls (d@(t, Syntax.NoInit pos id):ds) = do
                checkRedeclaration id
                lift $ assureProperType t
                checkTypeExists NoVoid t
                (nds, f) <- withVar id t (checkDecls ds)
                return (d:nds, f . addVar id t)
            checkDecls (d@(t, Syntax.Init pos id e):ds) = do
                checkRedeclaration id
                (ne, et) <- checkTypes e
                (nt, nne) <- case t of
                        Syntax.InfferedT _ -> case et of
                                        Syntax.InfferedT _ -> failure $ Errors.UnknownFailure $ "Type cannot be inffered from null"
                                        _ -> return (et, ne)
                        _ -> do
                            checkTypeExists NoVoid t
                            checkCastUp pos et t
                            (cls,_,_) <- ask
                            b <- lift $ equivalentType cls t et
                            if b then return (t, ne)
                            else return (t, Cast pos t ne)
                (nds, f) <- withVar id nt (checkDecls ds)
                return ((nt, Syntax.Init pos id nne):nds, f . addVar id nt)
            checkDecls [] = return ([], id)
    checkTypes (Syntax.Assignment pos ase e) = do
        (nase, aset) <- checkTypes ase
        checkEisLValue pos nase
        (ne, et) <- checkTypes e
        checkCastUp pos et aset
        (cls,_,_) <- ask
        b <- lift $ equivalentType cls et aset
        if b then return (Syntax.Assignment pos nase ne, id)
        else case aset of
                Syntax.IntT _ -> return (Syntax.Assignment pos nase (Cast pos aset ne), id)
                Syntax.ByteT _ -> return (Syntax.Assignment pos nase (Cast pos aset ne), id)
                _ -> return (Syntax.Assignment pos nase ne, id)
    checkTypes (Syntax.ReturnValue pos e) = do
        rt <- retrieve "$ret" >>= return . fromJust
        (ne, et) <- checkTypes e
        checkCastUp pos et rt
        (cls,_,_) <- ask
        b <- lift $ equivalentType cls et rt
        if b then return (Syntax.ReturnValue pos ne, id)
        else return (Syntax.ReturnValue pos (Cast pos rt ne), id)
    checkTypes (Syntax.ReturnVoid pos) = do
        rt <- retrieve "$ret" >>= return . fromJust
        case rt of
            Syntax.VoidT _ -> return (Syntax.ReturnVoid pos, id)
            _ -> failure $ Errors.UnknownFailure $ "Return is missing a value"
    checkTypes (Syntax.IfElse pos econd strue sfalse) = do
        (necond, econdt) <- checkTypes econd
        case econdt of
            Syntax.BoolT _ -> case strue of
                        Syntax.VarDecl pv _ -> failure $ Errors.UnknownFailure $ "Value declaration cannot be a single statement"
                        _ -> do
                            (nst, _) <- checkTypes strue
                            case sfalse of
                                Syntax.VarDecl pv _ -> failure $ Errors.UnknownFailure $ "Value declaration cannot be a single statement"
                                _ -> do
                                    (nsf, _) <- checkTypes sfalse
                                    return (Syntax.IfElse pos necond nst nsf, id)
            _ -> failure $ Errors.UnknownFailure $ "Expected boolean expression in condition, given "++typeName econdt
    checkTypes (Syntax.While pos econd stmt) = do
        (necond, econdt) <- checkTypes econd
        case econdt of
            Syntax.BoolT _ -> case stmt of
                        Syntax.VarDecl pv _ -> failure $ Errors.UnknownFailure $ "Value declaration cannot be a single statement"
                        _ -> do
                            (nst, _) <- checkTypes stmt
                            return (Syntax.While pos necond nst, id)
            _ -> failure $ Errors.UnknownFailure $ "Expected boolean expression in condition, given "++typeName econdt
    checkTypes (Syntax.ExprStmt pos e) = do
        (ne, _) <- checkTypes e
        return (Syntax.ExprStmt pos ne, id)
