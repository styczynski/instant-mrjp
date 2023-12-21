module Typings.TypeChecking where

import qualified Program.Syntax as Syntax
import Typings.Def
import Typings.Types as Type
import Reporting.Errors.Position

import Control.Monad.State hiding (void)

import Typings.InheritanceHierarchy
import Typings.Env

import Control.Lens
import qualified Data.Map as M
import qualified Reporting.Errors.Def as Errors

class TypeCheckable a where
    checkTypes :: a Position -> TypeChecker (a Position)

withVar :: Type.Name -> Type.Type -> TypeChecker x -> TypeChecker x
withVar name t m = do
    env <- tcEnv
    newEnv <- either (\(newName, prevName, prevType) -> todoImplementError $ "Redeclaration of " ++ (Type.stringName prevName)) (return) (addVar name t env)
    withStateT (const newEnv) m
    --withStateT (addVar name t) m

withSeparateScope :: Position -> TypeChecker x -> TypeChecker x
withSeparateScope pos = withStateT (separateScope pos)

withClassContext :: String -> TypeChecker x -> TypeChecker x
withClassContext className m = do
    -- TODO: Add class this to env here!! (see classEnv)
    (\cls -> withStateT (\env -> env & currentClass %~ (\_ -> Just cls)) m) =<< maybe (todoImplementError "Unknown class was used") return =<< findClass className

withFunctionContext :: String -> TypeChecker x -> TypeChecker x
withFunctionContext funcName m = do
    -- TODO: Add args to env here!!! (see funEnv)
    (\fn -> withStateT (\env -> env & currentFunction %~ (\_ -> Just fn)) m) =<< maybe (todoImplementError "Unknown function was used") return =<< tcEnvGet (flip findFunction funcName)

getContextFunctionReturnType :: TypeChecker Type.Type 
getContextFunctionReturnType = 
    maybe (todoImplementError "Invalid usage ??? out of function context") (\(Type.Fun _ retType _ _) -> return retType) =<< ((tcEnvGet $ (\env -> env^.currentFunction)))

checkArgsRedeclaration :: [Syntax.Arg Position] -> TypeChecker ()
checkArgsRedeclaration _ = return ()

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

checkCastUp :: Position -> Syntax.Type Position -> Syntax.Type Position -> TypeChecker ()
checkCastUp pos tFrom tTo = do
    c <- canBeCastUp tFrom tTo 
    if c then return ()
    else todoImplementError "Cannot convert types" --throw ("Cannot convert " ++ typeName tFrom ++ " to "++typeName tTo, pos)

typeFromArg :: Syntax.Arg Position -> TypeChecker Type.Type
typeFromArg (Syntax.Arg pos t id) = assureProperType t >> return t

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
    maybe (todoImplementError $ "Undeclared type "++n) (\_ -> return ()) =<< findClass n
        -- Just x -> return ()
        -- _ -> throw ("Undeclared type "++n, pos)
checkTypeExists _ (Syntax.ArrayT _ t) = checkTypeExists NoVoid t
checkTypeExists Type.NoVoid (Syntax.VoidT pos) = todoImplementError "Illegal use of type void"
checkTypeExists _ _ = return ()

equivalentType :: Syntax.Type Position -> Syntax.Type Position -> TypeChecker Bool
equivalentType t1 t2 = do
    a <- canBeCastUp t1 t2
    b <- canBeCastUp t2 t1
    return (a && b)

assureProperType :: Syntax.Type Position -> TypeChecker ()
assureProperType (Syntax.InfferedT pos) = todoImplementError "Inffered type instead of a proper type"
assureProperType _ = return ()

instance TypeCheckable Syntax.Program where
    checkTypes (Syntax.Program pos defs) = mapM checkTypes defs >>= return . Syntax.Program pos

instance TypeCheckable Syntax.Definition where
    checkTypes (Syntax.FunctionDef pos tret id@(Syntax.Ident _ funName) args b) = do
        checkTypeExists Type.AllowVoid tret
        mapM typeFromArg args >>= mapM_ (checkTypeExists NoVoid)
        checkArgsRedeclaration args
        --checkedBody <- local (funEnv tret args) (checkTypes b)
        checkedBody <- withFunctionContext funName (checkTypes b)
        return $ Syntax.FunctionDef pos tret id args checkedBody
    checkTypes (Syntax.ClassDef pos id@(Syntax.Ident namePos className) parent decls) = do
        checkTypeExists Type.NoVoid (Syntax.ClassT pos pid)
        checkedDecls <- withClassContext className (mapM checkTypes decls)
        return $ Syntax.ClassDef pos id (Syntax.justName pid) checkedDecls
        where
            pid = case parent of
                    (Syntax.Name _ x) -> x
                    (Syntax.NoName _) -> let (Syntax.Ident (Position f l c) s) = id in Syntax.Ident (Position f l (c+length s)) "Object"
            pos = let (Syntax.Ident p _) = pid in p

instance TypeCheckable Syntax.ClassDecl where
    checkTypes f@(Syntax.FieldDecl pos t id) = do
        checkTypeExists Type.NoVoid t
        return f
    checkTypes (Syntax.MethodDecl pos tret id@(Syntax.Ident _ methodName) args b) = do
        checkTypeExists AllowVoid tret
        mapM typeFromArg args >>= mapM_ (checkTypeExists NoVoid)
        checkArgsRedeclaration args
        -- TODO: should be separate function not withFunctionContext
        checkedBody <- withFunctionContext methodName (checkTypes b)
        return $ Syntax.MethodDecl pos tret id args checkedBody

instance TypeCheckable Syntax.Block where
    checkTypes (Syntax.Block pos stmts) = do
        newStmts <- withSeparateScope pos (mapM checkTypes stmts)
        return $ Syntax.Block pos newStmts

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
    checkTypes (Syntax.Empty pos) = return $ Syntax.Empty pos
    checkTypes (Syntax.BlockStmt pos b) = checkTypes b >>= \b -> return $ Syntax.BlockStmt pos b
    checkTypes (Syntax.VarDecl pos decls) = do
        ndecls <- checkDecls decls -- TODO: Handle f correctly Env -> Env?
        return $ Syntax.VarDecl pos ndecls
        where
            checkDecls :: [(Syntax.Type Position, Syntax.DeclItem Position)] -> TypeChecker [(Syntax.Type Position, Syntax.DeclItem Position)] 
            checkDecls (d@(t, Syntax.NoInit pos id):ds) = do
                --checkRedeclaration id
                assureProperType t
                checkTypeExists NoVoid t
                nds <- withVar id t (checkDecls ds)
                return (d:nds) -- TODO Handle env -> env? f . addVar id t
            checkDecls (d@(t, Syntax.Init pos id e):ds) = do
                --checkRedeclaration id
                (ne, et) <- checkE e
                (nt, nne) <- case t of
                        Syntax.InfferedT _ -> case et of
                                        Syntax.InfferedT _ -> todoImplementError $ "Type cannot be inffered from null"
                                        _ -> return (et, ne)
                        _ -> do
                            checkTypeExists NoVoid t
                            checkCastUp pos et t
                            b <- equivalentType t et
                            if b then return (t, ne)
                            else return (t, Syntax.Cast pos t ne)
                nds <- withVar id nt (checkDecls ds)
                return ((nt, Syntax.Init pos id nne):nds)
            checkDecls [] = return []
    checkTypes (Syntax.Assignment pos ase e) = do
        (nase, aset) <- checkE ase
        checkEisLValue pos nase
        (ne, et) <- checkE e
        checkCastUp pos et aset
        b <- equivalentType et aset
        if b then return $ Syntax.Assignment pos nase ne
        else case aset of
                Syntax.IntT _ -> return $ Syntax.Assignment pos nase (Syntax.Cast pos aset ne)
                Syntax.ByteT _ -> return $ Syntax.Assignment pos nase (Syntax.Cast pos aset ne)
                _ -> return $ Syntax.Assignment pos nase ne
    checkTypes (Syntax.ReturnValue pos e) = do
        rt <- getContextFunctionReturnType
        (ne, et) <- checkE e
        checkCastUp pos et rt
        b <- equivalentType et rt
        if b then return $ Syntax.ReturnValue pos ne
        else return $ Syntax.ReturnValue pos (Syntax.Cast pos rt ne)
    checkTypes (Syntax.ReturnVoid pos) = do
        rt <- getContextFunctionReturnType
        case rt of
            Syntax.VoidT _ -> return $ Syntax.ReturnVoid pos
            _ -> todoImplementError $ "Return is missing a value"
    checkTypes (Syntax.IfElse pos econd strue sfalse) = do
        (necond, econdt) <- checkE econd
        case econdt of
            Syntax.BoolT _ -> case strue of
                        Syntax.VarDecl pv _ -> todoImplementError $ "Value declaration cannot be a single statement"
                        _ -> do
                            nst <- checkTypes strue
                            case sfalse of
                                Syntax.VarDecl pv _ -> todoImplementError $ "Value declaration cannot be a single statement"
                                _ -> do
                                    nsf <- checkTypes sfalse
                                    return $ Syntax.IfElse pos necond nst nsf
            _ -> todoImplementError $ "Expected boolean expression in condition, given " -- ++typeName econdt
    checkTypes (Syntax.While pos econd stmt) = do
        (necond, econdt) <- checkE econd
        case econdt of
            Syntax.BoolT _ -> case stmt of
                        Syntax.VarDecl pv _ -> todoImplementError $ "Value declaration cannot be a single statement"
                        _ -> do
                            nst <- checkTypes stmt
                            return $ Syntax.While pos necond nst
            _ -> todoImplementError $ "Expected boolean expression in condition, given " -- ++typeName econdt
    checkTypes (Syntax.ExprStmt pos e) = do
        (ne, _) <- checkE e
        return $ Syntax.ExprStmt pos ne

-- TODO: Implement
checkEisLValue :: Position -> Syntax.Expr Position -> TypeChecker ()
checkEisLValue pos _ = return ()

checkE :: Syntax.Expr Position -> TypeChecker (Syntax.Expr Position, Type.Type)
checkE a = return (a, Syntax.VoidT Undefined)