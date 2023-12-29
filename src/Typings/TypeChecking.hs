{-# LANGUAGE TypeApplications #-}
module Typings.TypeChecking where

import qualified Program.Syntax as Syntax
import Typings.Def
import Typings.Types as Type
import Reporting.Errors.Position
import Utils.Similarity

import Control.Monad.State hiding (void)

import Typings.InheritanceHierarchy
import Typings.Env

import Control.Lens
import Data.Generics.Product
import GHC.Generics
import qualified Data.Map as M
import qualified Reporting.Errors.Def as Errors
import Reporting.Logs

void = Syntax.VoidT BuiltIn
bool = Syntax.BoolT BuiltIn
int = Syntax.IntT BuiltIn
byte = Syntax.ByteT BuiltIn
name = Syntax.Ident BuiltIn
string = Syntax.StringT BuiltIn
array = Syntax.ArrayT BuiltIn
object = class_ "Object"
class_ s = Syntax.ClassT BuiltIn (name s)

class (Syntax.IsSyntax a Position) => TypeCheckable a where
    doCheckTypes :: a Position -> TypeChecker (a Position)
    doInferType :: a Position -> TypeChecker (a Position, Type.Type)
    doInferType ast = (\newAst -> return (newAst, Syntax.VoidT Undefined)) =<< checkTypes ast

    checkTypes :: a Position -> TypeChecker (a Position)
    checkTypes ast = do
        tcEnvSet (\env -> env & inferTrace .~ initialTrace)
        doCheckTypes ast
        --withStateT (\env -> env^.debugTypings %~ M.insert pos ) (doCheckTypes ast)

    inferType :: a Position -> TypeChecker (a Position, Type.Type)
    inferType ast = do
        e <- tcEnv
        liftPipelineTC $ printLogInfoStr $ "-> " ++ (show $ Syntax.getPos ast) ++ " " ++ (show $ e^.inferTrace.inferStack) ++ (show $ e^.inferTrace.inferChildren)
        tcEnvSet (inferTraceEnter (Syntax.getPos ast))
        (newAst, astType) <- doInferType ast
        --tcEnvSet (\env -> env & debugTypings %~ M.insert (Syntax.getPos ast) astType)
        tcEnvSet (inferTraceQuit (Syntax.getPos ast) astType)
        liftPipelineTC $ printLogInfoStr $ "<- " ++ (show $ Syntax.getPos ast)
        return (newAst, astType)


withVar :: Position -> (TypeCheckerEnv -> Position -> (Type.Name, Type.Type) -> (Type.Name, Type.Type) -> Errors.Error) -> Type.Name -> Type.Type -> TypeChecker x -> TypeChecker x
withVar sourcePosition errorHandler name t m = do
    env <- tcEnv
    newEnv <- either (\(newName, prevName, prevType) -> failure $ errorHandler env sourcePosition (name, t) (prevName, prevType)) (return) (addVar name t env)
    withStateT (const newEnv) m
    --withStateT (addVar name t) m

getFunction :: String -> TypeChecker (Maybe Type.Function)
getFunction name = tcEnvGet ((flip findFunction) name)

getVar :: String -> TypeChecker (Maybe Type.Type)
getVar name = (maybe (return Nothing) (return . Just . snd)) =<< (tcEnvGet $ lookupVar name)

withSeparateScope :: Position -> TypeChecker x -> TypeChecker x
withSeparateScope pos m =
    withStateT revertScope . return =<< withStateT (separateScope pos) m

withClassContext :: String -> TypeChecker x -> TypeChecker x
withClassContext className m = do
    -- TODO: Add class this to env here!! (see classEnv)
    (\cls -> withStateT (\env -> env & currentClass %~ (\_ -> Just cls)) m) =<< maybe (todoImplementError "Unknown class was used") return =<< findClass className

withMethodContext :: String -> TypeChecker x -> TypeChecker x
withMethodContext methodName m = do
        env <- tcEnv
        member <- tcEnvGet ((flip findMember) methodName)
        case member of
                (Just (Type.Method methodName methodType args (Syntax.MethodDecl a t id as b))) -> do
                    (Type.Class (Syntax.Ident pos _) _ _ currentClassDecl) <- maybe (todoImplementError "withMethodContext: missing current class") (return) $ env^.currentClass
                    withSeparateScope pos (methodContext methodName methodType args (Syntax.FunctionDef a t id as b) m)
                _ -> todoImplementError "withMethodContext: Missing member?"
    where
        methodContext :: Type.Name -> Type.Type -> (M.Map String (Type.Name, Type.Type)) -> (Syntax.Definition Position) -> TypeChecker x -> TypeChecker x
        methodContext methodName methodType args currentClassDecl m = do
            env <- tcEnv
            newEnv <- foldM (\accEnv (varName, varType) -> either (\_ -> todoImplementError "withMethodContext: duplicate arg?") (return) $ addVar varName varType accEnv) env args
            withStateT (\env -> env & currentFunction .~ Nothing) . return =<< withStateT (const $ newEnv & currentFunction %~ (\_ -> Just $ (Type.Fun methodName methodType args currentClassDecl))) m 
 

withFunctionContext :: String -> TypeChecker x -> TypeChecker x
withFunctionContext funcName m = do
    -- TODO: Add args to env here!!! (see funEnv)
    fn@(Type.Fun _ _ args _) <- maybe (todoImplementError $ "Unknown function was used: " ++ funcName) return =<< tcEnvGet (flip findFunction funcName)
    --withStateT (\env -> env & currentFunction %~ (\_ -> Just fn)) m
    env <- tcEnv
    -- either (\_ -> return env) (return) $ addVar varName varType env
    newEnv <- foldM (\env (varName, varType) -> either (\_ -> todoImplementError "withFunctionContext: Duplicate arg?") (return) $ addVar varName varType env) env args
    withStateT (\env -> env & currentFunction .~ Nothing) . return =<< withStateT (const $ newEnv & currentFunction %~ (\_ -> Just fn)) m
    --withStateT (\env -> M.fold (\(varName, varType) env -> addVar varName varType env) env args)

getContextClassType :: TypeChecker (Maybe Type.Type)
getContextClassType =
    maybe (return Nothing) (\(Type.Class className@(Syntax.Ident pos _) _ _ _) -> return $ Just $ Syntax.ClassT pos className) =<< ((tcEnvGet $ (\env -> env^.currentClass)))

getContextFunction :: TypeChecker Type.Function
getContextFunction =
    maybe (todoImplementError "Invalid usage ??? out of function context") (return) =<< ((tcEnvGet $ (\env -> env^.currentFunction)))

getContextFunctionReturnType :: TypeChecker Type.Type
getContextFunctionReturnType =
    maybe (todoImplementError "Invalid usage ??? out of function context") (\(Type.Fun _ retType _ _) -> return retType) =<< ((tcEnvGet $ (\env -> env^.currentFunction)))

checkArgsRedeclaration :: (TypeCheckerEnv -> (Type.Function) -> (Syntax.Arg Position) -> [(Syntax.Arg Position)] -> Errors.Error) -> [Syntax.Arg Position] -> TypeChecker ()
checkArgsRedeclaration errorHandler args = 
    mapM_ (\elem@(Syntax.Arg _ _ (Syntax.Ident _ elemName)) -> checkError errorHandler elem $ filter (\other@(Syntax.Arg _ _ (Syntax.Ident _ otherName)) -> elemName == otherName && elem /= other) args) args
    where
        checkError :: (TypeCheckerEnv -> (Type.Function) -> (Syntax.Arg Position) -> [(Syntax.Arg Position)] -> Errors.Error) -> (Syntax.Arg Position) -> [(Syntax.Arg Position)] -> TypeChecker ()
        checkError errorHandler arg [] = return ()
        checkError errorHandler arg duplicates = failure =<< tcEnvGet (\env -> maybe (Errors.UnknownFailure env "checkArgsRedeclaration: No current function") (\fn -> errorHandler env fn arg duplicates) $ env^.currentFunction)

getMemberType :: Syntax.Ident Position -> Syntax.Ident Position -> TypeChecker (Maybe (Type.Type))
getMemberType _ _ = return Nothing

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

checkCastUpErr :: (TypeCheckerEnv -> Type.Type -> Type.Type -> Errors.Error) -> Position -> Syntax.Type Position -> Syntax.Type Position -> TypeChecker ()
checkCastUpErr errorHandler pos tFrom tTo = do
    c <- canBeCastUp tFrom tTo
    if c then return ()
    else (\env -> failure $ errorHandler env tFrom tTo) =<< tcEnv
    --else todoImplementError $ "Cannot convert types " ++ (printi 0 tFrom) ++ " to " ++ (printi 0 tTo) --throw ("Cannot convert " ++ printi 0 tFrom ++ " to "++printi 0 tTo, pos)

checkCastUp :: Position -> Syntax.Type Position -> Syntax.Type Position -> TypeChecker ()
checkCastUp pos tFrom tTo = do
    c <- canBeCastUp tFrom tTo
    if c then return ()
    else todoImplementError $ "Cannot convert types " ++ (printi 0 tFrom) ++ " to " ++ (printi 0 tTo) --throw ("Cannot convert " ++ printi 0 tFrom ++ " to "++printi 0 tTo, pos)

typeFromArg :: Syntax.Arg Position -> TypeChecker (Type.Type, Syntax.Arg Position)
typeFromArg arg@(Syntax.Arg pos t id) = assureProperType t >> return (t, arg)

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

checkTypeExists :: Type.AllowVoid -> Errors.TypeContext -> Syntax.Type Position -> TypeChecker ()
checkTypeExists _ _ (Syntax.ClassT _ id@(Syntax.Ident pos n)) = do
    env <- tcEnv
    maybe (failure $ Errors.UnknownType env id) (\_ -> return ()) =<< findClass n
        -- Just x -> return ()
        -- _ -> throw ("Undeclared type "++n, pos)
checkTypeExists _ typeContext (Syntax.ArrayT _ t) = checkTypeExists NoVoid typeContext t
checkTypeExists Type.NoVoid typeContext invalidType@(Syntax.VoidT _) = do
    env <- tcEnv
    failure $ Errors.IllegalTypeUsed env typeContext invalidType
checkTypeExists _ _ _ = return ()

equivalentType :: Syntax.Type Position -> Syntax.Type Position -> TypeChecker Bool
equivalentType t1 t2 = do
    a <- canBeCastUp t1 t2
    b <- canBeCastUp t2 t1
    return (a && b)

assureProperType :: Syntax.Type Position -> TypeChecker ()
assureProperType (Syntax.InfferedT pos) = todoImplementError "Inffered type instead of a proper type"
assureProperType _ = return ()

instance TypeCheckable Syntax.Program where
    doCheckTypes (Syntax.Program pos defs) = mapM checkTypes defs >>= return . Syntax.Program pos

instance TypeCheckable Syntax.Definition where
    doCheckTypes fn@(Syntax.FunctionDef pos tret id@(Syntax.Ident _ funName) args b) = do
        checkTypeExists Type.AllowVoid (Errors.TypeInFunctionReturn fn) tret
        mapM typeFromArg args >>= mapM_ (\(t, arg) -> checkTypeExists NoVoid (Errors.TypeInFunctionArgDecl fn arg) t)
        --checkedBody <- local (funEnv tret args) (checkTypes b)
        checkedBody <- withFunctionContext funName (checkArgsRedeclaration (Errors.DuplicateFunctionArgument) args >> checkTypesFunctionBlock b)
        return $ Syntax.FunctionDef pos tret id args checkedBody
    doCheckTypes cls@(Syntax.ClassDef pos id@(Syntax.Ident namePos className) parent decls) = do
        checkTypeExists Type.NoVoid (Errors.TypeInClassParent cls) (Syntax.ClassT pos pid)
        checkedDecls <- withClassContext className (mapM checkTypes decls)
        return $ Syntax.ClassDef pos id (Syntax.justName pid) checkedDecls
        where
            pid = case parent of
                    (Syntax.Name _ x) -> x
                    (Syntax.NoName _) -> let (Syntax.Ident (Position tid f l c) s) = id in Syntax.Ident (Position tid f l (c+length s)) "Object"
            pos = let (Syntax.Ident p _) = pid in p

instance TypeCheckable Syntax.ClassDecl where
    doCheckTypes f@(Syntax.FieldDecl pos t id) = do
        checkTypeExists Type.NoVoid (Errors.TypeInClassField f) t
        return f
    doCheckTypes method@(Syntax.MethodDecl pos tret id@(Syntax.Ident _ methodName) args b) = do
        checkTypeExists AllowVoid (Errors.TypeInMethodReturn method) tret
        mapM typeFromArg args >>= mapM_ (\(t, arg) -> checkTypeExists NoVoid (Errors.TypeInMethodArgDecl method arg) t)
        --checkArgsRedeclaration args
        -- TODO: should be separate function not withFunctionContext
        checkedBody <- withMethodContext methodName (checkArgsRedeclaration (Errors.DuplicateFunctionArgument) args >> checkTypesFunctionBlock b)
        -- checkedBody <- withFunctionContext methodName (checkTypes b)
        return $ Syntax.MethodDecl pos tret id args checkedBody

instance TypeCheckable Syntax.Block where
    doCheckTypes (Syntax.Block pos stmts) = do
        newStmts <- withSeparateScope pos (mapM checkTypes stmts)
        return $ Syntax.Block pos newStmts

checkTypesFunctionBlock :: Syntax.Block Position -> TypeChecker (Syntax.Block Position)
checkTypesFunctionBlock (Syntax.Block pos stmts) = do
    newStmts <- mapM checkTypes stmts
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
    doCheckTypes (Syntax.Empty pos) = return $ Syntax.Empty pos
    doCheckTypes (Syntax.BlockStmt pos b) = checkTypes b >>= \b -> return $ Syntax.BlockStmt pos b
    doCheckTypes decl@(Syntax.VarDecl pos decls) = do
        ndecls <- checkDecls pos decls -- TODO: Handle f correctly Env -> Env?
        return $ Syntax.VarDecl pos ndecls
        where
            checkDecls :: Position -> [(Syntax.Type Position, Syntax.DeclItem Position)] -> TypeChecker [(Syntax.Type Position, Syntax.DeclItem Position)]
            checkDecls srcPos (d@(t, declItem@(Syntax.NoInit pos id)):ds) = do
                --checkRedeclaration id
                assureProperType t
                checkTypeExists NoVoid (Errors.TypeInVarDecl declItem) t
                nds <- withVar srcPos Errors.VariableRedeclared id t (checkDecls srcPos ds)
                return (d:nds) -- TODO Handle env -> env? f . addVar id t
            checkDecls srcPos (d@(t, declItem@(Syntax.Init pos id e)):ds) = do
                --checkRedeclaration id
                (ne, et) <- inferType e
                (nt, nne) <- case t of
                        Syntax.InfferedT _ -> case et of
                                        Syntax.InfferedT _ -> todoImplementError $ "Type cannot be inffered from null"
                                        _ -> return (et, ne)
                        _ -> do
                            checkTypeExists NoVoid (Errors.TypeInVarDecl declItem) t
                            checkCastUpErr (\env -> Errors.IncompatibleTypesInit env (snd d)) pos et t
                            b <- equivalentType t et
                            if b then return (t, ne)
                            else return (t, Syntax.Cast pos t ne)
                nds <- withVar srcPos Errors.VariableRedeclared id nt (checkDecls srcPos ds)
                return ((nt, Syntax.Init pos id nne):nds)
            checkDecls _ [] = return []
    doCheckTypes stmt@(Syntax.Assignment pos ase e) = do
        (nase, aset) <- inferType ase
        checkEisLValue pos nase
        (ne, et) <- inferType e
        checkCastUpErr (\env -> Errors.IncompatibleTypesAssign env stmt) pos et aset
        b <- equivalentType et aset
        if b then return $ Syntax.Assignment pos nase ne
        else case aset of
                Syntax.IntT _ -> return $ Syntax.Assignment pos nase (Syntax.Cast pos aset ne)
                Syntax.ByteT _ -> return $ Syntax.Assignment pos nase (Syntax.Cast pos aset ne)
                _ -> return $ Syntax.Assignment pos nase ne
    doCheckTypes stmt@(Syntax.ReturnValue pos e) = do
        rt <- getContextFunctionReturnType
        fn <- getContextFunction
        (ne, et) <- inferType e
        checkCastUpErr (\env -> Errors.IncompatibleTypesReturn env stmt fn) pos et rt
        b <- equivalentType et rt
        if b then return $ Syntax.ReturnValue pos ne
        else return $ Syntax.ReturnValue pos (Syntax.Cast pos rt ne)
    doCheckTypes (Syntax.ReturnVoid pos) = do
        rt <- getContextFunctionReturnType
        case rt of
            Syntax.VoidT _ -> return $ Syntax.ReturnVoid pos
            _ -> do
                fn <- getContextFunction
                env <- tcEnv
                failure $ Errors.MissingReturnValue env pos rt fn
    doCheckTypes (Syntax.IfElse pos econd strue sfalse) = do
        (necond, econdt) <- inferType econd
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
            _ -> todoImplementError $ "Expected boolean expression in condition, given " -- ++printi 0 econdt
    doCheckTypes (Syntax.While pos econd stmt) = do
        (necond, econdt) <- inferType econd
        case econdt of
            Syntax.BoolT _ -> case stmt of
                        Syntax.VarDecl pv _ -> todoImplementError $ "Value declaration cannot be a single statement"
                        _ -> do
                            nst <- checkTypes stmt
                            return $ Syntax.While pos necond nst
            _ -> todoImplementError $ "Expected boolean expression in condition, given " -- ++printi 0 econdt
    doCheckTypes (Syntax.ExprStmt pos e) = do
        (ne, _) <- inferType e
        return $ Syntax.ExprStmt pos ne

-- TODO: Implement
checkEisLValue :: Position -> Syntax.Expr Position -> TypeChecker ()
checkEisLValue pos _ = return ()

instance TypeCheckable Syntax.Expr where
    doCheckTypes expr = (return . fst) =<< inferType expr

    doInferType (Syntax.Lit pos l@(Syntax.Int _ i)) =
        if i < 256 && i >= 0 then return (Syntax.Lit pos (Syntax.Byte pos i), Syntax.ByteT pos)
        else if i < 2^31 && i >= -(2^31) then return (Syntax.Lit pos l, Syntax.IntT pos)
        else todoImplementError "Constant exceeds the size of int"
    doInferType (Syntax.Lit pos l@(Syntax.String _ _)) = return (Syntax.Lit pos l, Syntax.StringT pos)
    doInferType (Syntax.Lit pos l@(Syntax.Bool _ _)) = return (Syntax.Lit pos l, Syntax.BoolT pos)
    doInferType (Syntax.Lit pos l@(Syntax.Byte _ _)) = return (Syntax.Lit pos l, Syntax.ByteT pos)
    doInferType (Syntax.Lit pos l@(Syntax.Null _)) = return (Syntax.Lit pos l, Syntax.InfferedT pos)
    doInferType (Syntax.Var pos id@(Syntax.Ident _ varName)) = do
        mv <- getVar varName
        case mv of
            Just t -> return (Syntax.Var pos id, t)
            Nothing -> do
                mc <- getContextClassType
                case mc of
                    Just (Syntax.ClassT _ idc@(Syntax.Ident _ clsName)) -> do
                        mm <- getMemberType idc id
                        case mm of
                            Just t -> return (Syntax.Member pos (Syntax.Var pos (Syntax.Ident pos "this")) id (Just clsName), t)
                            Nothing -> do
                                mf <- getFun id
                                case mf of
                                    Just t -> return (Syntax.Var pos id, t)
                                    Nothing -> err id
                    Nothing -> do
                        mf <- getFun id
                        case mf of
                            Just t -> return (Syntax.Var pos id, t)
                            Nothing -> err id
        where
            getFun id@(Syntax.Ident _ name) = do
                mf <- getFunction name
                case mf of
                    Just fn@(Type.Fun (Syntax.Ident p _) t ts _) -> return . Just $ Syntax.FunT p t $ funcArgsTypes fn
                    _ -> err id
            err id@(Syntax.Ident pos name) = do
                env <- tcEnv
                failure $ Errors.UnknownVariable env id --todoImplementError ("Undefined identifier: "++name)
            elemF i@(Syntax.Ident _ name) (f@(Type.Fun (Syntax.Ident _ n) _ _ _):xs) =
                if name == n then Just f
                else elemF i xs
            elemF _ [] = Nothing
    doInferType (Syntax.App pos efun es) = do
        (nef, eft) <- inferType efun
        case eft of
            fn@(Syntax.FunT _ ret args) -> do
                nes <- mapM inferType es
                let efts = map snd nes
                env <- tcEnv
                if length efts > length args then
                    failure $ Errors.CallTooManyParameters env nef fn nes
                    --todoImplementError ("Too many arguments")
                else if length efts < length args then
                    todoImplementError ("Too few arguments")
                else return ()
                mapM_ (\(l,(e,r)) -> checkCastUp (e ^. position @1) r l) $ zip args nes
                return (Syntax.App pos nef (map fst nes), ret)
            _ -> todoImplementError ("Expected a function or a method, given"++printi 0 eft)
    doInferType cast@(Syntax.Cast pos t e) = do
        checkTypeExists Type.NoVoid (Errors.TypeInCast cast) t
        case t of
            Syntax.InfferedT _ -> todoImplementError ("Invalid type in cast expression")
            _ -> do
                (ne, et) <- inferType e
                c <- canBeCastDown et t
                if c then
                    case ne of
                        (Syntax.Cast _ _ ie) -> return (Syntax.Cast pos t ie, t)
                        (Syntax.Lit _ (Syntax.Null _)) -> return (ne, t)
                        _ -> return (Syntax.Cast pos t ne, t)
                else todoImplementError ("Illegal cast of "++printi 0 et++" to "++printi 0 t)
    doInferType (Syntax.ArrAccess pos earr ein _) = do
        (nearr, art) <- inferType earr
        case art of
            Syntax.ArrayT _ t -> do
                (nein, et) <- inferType ein
                case et of
                    Syntax.IntT _ -> return (Syntax.ArrAccess pos nearr nein (Just t), t)
                    Syntax.ByteT _ -> return (Syntax.ArrAccess pos nearr (Syntax.Cast pos int nein) (Just t), t)
                    _ -> todoImplementError ("Expected a numerical index, given "++printi 0 et)
            _ -> todoImplementError ("Expected array type, given "++printi 0 art)
    doInferType stmt@(Syntax.NewObj pos t m) = do
        checkTypeExists NoVoid (Errors.TypeInNew stmt) t
        case m of
            Nothing -> do
                case t of
                    Syntax.ClassT _ (Syntax.Ident _ n) -> if n /= "String" then return ()
                    else todoImplementError ("Cannot instantiate an empty String")
                    Syntax.StringT _ -> todoImplementError ("Cannot instantiate an empty String")
                    _ -> todoImplementError ("Expected a class")
                return (Syntax.NewObj pos t m, t)
            Just e -> do
                (ne, et) <- inferType e
                b <- canBeCastUp et int
                if b then return (Syntax.NewObj pos t (Just ne), Syntax.ArrayT pos t)
                else todoImplementError ("Expected a numerical size in array constructor, given "++printi 0 et)
    doInferType (Syntax.Member pos e id _) = do
        (ne, et) <- inferType e
        case et of
            Syntax.StringT _ -> cont pos ne id (name "String")
            Syntax.ArrayT _ _ -> cont pos ne id (name "Array")
            Syntax.ClassT _ name -> cont pos ne id name
            Syntax.InfferedT _ -> cont pos ne id (name "Object")
            _ -> todoImplementError ("Expected an object, given "++printi 0 et)
        where
            cont pos e id@(Syntax.Ident p i) cls@(Syntax.Ident _ clsName) = do
                if clsName == "Array" && (i == "elements" || i == "elementSize") then
                    todoImplementError ("Undefined member "++i)
                else do
                    mem <- getMemberType cls id
                    case mem of
                        Just t -> return (Syntax.Member pos e id (Just clsName), t)
                        Nothing -> todoImplementError ("Undefined member "++i)
    doInferType (Syntax.UnaryOp pos op e) = do
        (ne, et) <- inferType e
        case (op, et) of
            (Syntax.Not _, Syntax.BoolT _) -> return (Syntax.UnaryOp pos op ne, et)
            (Syntax.Neg _, Syntax.IntT _) -> return (Syntax.UnaryOp pos op ne, et)
            (Syntax.Neg _, Syntax.ByteT _) -> return (Syntax.UnaryOp pos op ne, et)
            (Syntax.Not _, _) -> todoImplementError ("Expected boolean expression, given "++printi 0 et)
            _ -> todoImplementError ("Expected a number, given "++printi 0 et)
    doInferType (Syntax.BinaryOp pos op el er) = do
        (nel, elt) <- inferType el
        (ner, ert) <- inferType er
        let err = todoImplementError ("Incompatible operands' types: "++printi 0 elt++" and "++printi 0 ert)
        case (op, fmap (\_->()) elt, fmap (\_->()) ert) of
            (Syntax.Add _, Syntax.ClassT _ (Syntax.Ident _ "String"), Syntax.ClassT _ (Syntax.Ident _ "String")) -> return (Syntax.App pos (Syntax.Member pos nel (name "concat") (Just "String")) [ner], elt)
            (Syntax.Add _, Syntax.StringT _, Syntax.ClassT _ (Syntax.Ident _ "String")) -> return (Syntax.App pos (Syntax.Member pos nel (name "concat") (Just "String")) [ner], elt)
            (Syntax.Add _, Syntax.ClassT _ (Syntax.Ident _ "String"), Syntax.StringT _) -> return (Syntax.App pos (Syntax.Member pos nel (name "concat") (Just "String")) [ner], ert)
            (Syntax.Add _, Syntax.StringT _, Syntax.StringT _) -> return (Syntax.App pos (Syntax.Member pos nel (name "concat") (Just "String")) [ner], ert)
            (Syntax.Add _, Syntax.StringT _, Syntax.ClassT _ _) -> inferType (Syntax.App pos (Syntax.Member pos nel (name "concat") (Just "String")) [Syntax.App pos (Syntax.Member BuiltIn ner (name "toString") Nothing) []])
            (Syntax.Add _, Syntax.StringT _, Syntax.ArrayT _ _) -> inferType (Syntax.App pos (Syntax.Member pos nel (name "concat") (Just "String")) [Syntax.App pos (Syntax.Member BuiltIn ner (name "toString") Nothing) []])
            (Syntax.Add _, Syntax.StringT _, Syntax.BoolT _) -> inferType (Syntax.App pos (Syntax.Member pos nel (name "concat") (Just "String")) [Syntax.App pos (Syntax.Var BuiltIn (name "boolToString")) [ner]])
            (Syntax.Add _, Syntax.StringT _, Syntax.IntT _) -> inferType (Syntax.App pos (Syntax.Member pos nel (name "concat") (Just "String")) [Syntax.App pos (Syntax.Var BuiltIn (name "intToString")) [ner]])
            (Syntax.Add _, Syntax.StringT _, Syntax.ByteT _) -> inferType (Syntax.App pos (Syntax.Member pos nel (name "concat") (Just "String")) [Syntax.App pos (Syntax.Var BuiltIn (name "byteToString")) [ner]])
            (Syntax.Add _, Syntax.ClassT _ (Syntax.Ident _ "String"), Syntax.ClassT _ _) -> inferType (Syntax.App pos (Syntax.Member pos nel (name "concat") (Just "String")) [Syntax.App pos (Syntax.Member BuiltIn ner (name "toString") Nothing) []])
            (Syntax.Add _, Syntax.ClassT _ (Syntax.Ident _ "String"), Syntax.BoolT _) -> inferType (Syntax.App pos (Syntax.Member pos nel (name "concat") (Just "String")) [Syntax.App pos (Syntax.Var BuiltIn (name "boolToString")) [ner]])
            (Syntax.Add _, Syntax.ClassT _ (Syntax.Ident _ "String"), Syntax.IntT _) -> inferType (Syntax.App pos (Syntax.Member pos nel (name "concat") (Just "String")) [Syntax.App pos (Syntax.Var BuiltIn (name "intToString")) [ner]])
            (Syntax.Add _, Syntax.ClassT _ (Syntax.Ident _ "String"), Syntax.ByteT _) -> inferType (Syntax.App pos (Syntax.Member pos nel (name "concat") (Just "String")) [Syntax.App pos (Syntax.Var BuiltIn (name "intToString")) [ner]])
            (Syntax.Equ _, Syntax.ClassT _ (Syntax.Ident _ "String"), Syntax.StringT _) -> return (Syntax.App pos (Syntax.Member pos nel (name "equals") (Just "String")) [ner], bool)
            (Syntax.Equ _, Syntax.StringT _, Syntax.ClassT _ (Syntax.Ident _ "String")) -> return (Syntax.App pos (Syntax.Member pos nel (name "equals") (Just "String")) [ner], bool)
            (Syntax.Equ _, Syntax.StringT _, Syntax.StringT _) -> return (Syntax.App pos (Syntax.Member pos nel (name "equals") (Just "String")) [ner], bool)
            (Syntax.Neq _, Syntax.ClassT _ (Syntax.Ident _ "String"), Syntax.StringT _) -> return (Syntax.UnaryOp pos (Syntax.Not pos) (Syntax.App pos (Syntax.Member pos nel (name "equals") (Just "String")) [ner]), bool)
            (Syntax.Neq _, Syntax.StringT _, Syntax.ClassT _ (Syntax.Ident _ "String")) -> return (Syntax.UnaryOp pos (Syntax.Not pos) (Syntax.App pos (Syntax.Member pos nel (name "equals") (Just "String")) [ner]), bool)
            (Syntax.Neq _, Syntax.StringT _, Syntax.StringT _) -> return (Syntax.UnaryOp pos (Syntax.Not pos) (Syntax.App pos (Syntax.Member pos nel (name "equals") (Just "String")) [ner]), bool)
            (Syntax.Equ _, Syntax.ClassT _ (Syntax.Ident _ c), Syntax.ClassT _ _) -> return (Syntax.App pos (Syntax.Member pos nel (name "equals") (Just c)) [ner], bool)
            (Syntax.Neq _, Syntax.ClassT _ (Syntax.Ident _ c), Syntax.ClassT _ _) -> return (Syntax.UnaryOp pos (Syntax.Not pos) (Syntax.App pos (Syntax.Member pos nel (name "equals") (Just c)) [ner]), bool)
            (Syntax.Equ _, Syntax.ClassT _ (Syntax.Ident _ c), Syntax.StringT _) -> return (Syntax.App pos (Syntax.Member pos nel (name "equals") (Just c)) [ner], bool)
            (Syntax.Neq _, Syntax.ClassT _ (Syntax.Ident _ c), Syntax.StringT _) -> return (Syntax.UnaryOp pos (Syntax.Not pos) (Syntax.App pos (Syntax.Member pos nel (name "equals") (Just c)) [ner]), bool)
            (Syntax.Equ _, Syntax.StringT _, Syntax.ClassT _ _) -> return (Syntax.App pos (Syntax.Member pos nel (name "equals") (Just "String")) [ner], bool)
            (Syntax.Neq _, Syntax.StringT _, Syntax.ClassT _ _) -> return (Syntax.UnaryOp pos (Syntax.Not pos) (Syntax.App pos (Syntax.Member pos nel (name "equals") (Just "String")) [ner]), bool)
            (Syntax.Equ _, Syntax.InfferedT _, Syntax.ClassT _ _) -> return (Syntax.BinaryOp pos op nel ner, bool)
            (Syntax.Equ _, Syntax.ClassT _ _, Syntax.InfferedT _) -> return (Syntax.BinaryOp pos op nel ner, bool)
            (Syntax.Neq _, Syntax.InfferedT _, Syntax.ClassT _ _) -> return (Syntax.BinaryOp pos op nel ner, bool)
            (Syntax.Neq _, Syntax.ClassT _ _, Syntax.InfferedT _) -> return (Syntax.BinaryOp pos op nel ner, bool)
            (op, a, b) ->
                if a == b then
                    case a of
                        Syntax.ClassT _ (Syntax.Ident _ c) -> err
                        Syntax.VoidT _ -> err
                        Syntax.ByteT _ ->
                            case op of
                                Syntax.Div _ -> inferType (Syntax.BinaryOp pos op (Syntax.Cast pos (Syntax.IntT pos) nel) (Syntax.Cast pos (Syntax.IntT pos) ner))
                                Syntax.Mod _ -> inferType (Syntax.BinaryOp pos op (Syntax.Cast pos (Syntax.IntT pos) nel) (Syntax.Cast pos (Syntax.IntT pos) ner))
                                _ -> return (Syntax.BinaryOp pos op nel ner, opType elt op)
                        _ -> return (Syntax.BinaryOp pos op nel ner, opType elt op)
                else if a == Syntax.IntT () && b == Syntax.ByteT () then
                    inferType (Syntax.BinaryOp pos op nel (Syntax.Cast pos (Syntax.IntT pos) ner))
                else if a == Syntax.ByteT () && b == Syntax.IntT () then
                    inferType (Syntax.BinaryOp pos op (Syntax.Cast pos (Syntax.IntT pos) nel) ner)
                else err
        where
            opType t (Syntax.Equ _) = bool
            opType t (Syntax.Neq _) = bool
            opType t (Syntax.Lt _) = bool
            opType t (Syntax.Le _) = bool
            opType t (Syntax.Gt _) = bool
            opType t (Syntax.Ge _) = bool
            opType t _ = t