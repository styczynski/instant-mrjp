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
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
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

class (PrettyPrint (a Position), Syntax.IsSyntax a Position) => TypeCheckable a where
    doCheckTypes :: a Position -> TypeChecker (a Position)
    doInferType :: a Position -> TypeChecker (a Position, Type.Type)
    doInferType ast = (\newAst -> return (newAst, Syntax.VoidT Undefined)) =<< checkTypes ast

    checkTypes :: a Position -> TypeChecker (a Position)
    checkTypes ast = do
        tcEnvSet (\env -> env & inferTrace .~ initialTrace)
        liftPipelineTC $ printLogInfoStr $ "before -> " ++ (printi 0 ast)
        r <- doCheckTypes ast
        liftPipelineTC $ printLogInfoStr $ "after  <- " ++ (printi 0 r)
        return r
        --withStateT (\env -> env^.debugTypings %~ M.insert pos ) (doCheckTypes ast)

    inferType :: a Position -> TypeChecker (a Position, Type.Type)
    inferType ast = do
        e <- tcEnv
        --liftPipelineTC $ printLogInfoStr $ "-> " ++ (show $ Syntax.getPos ast) ++ " " ++ (show $ e^.inferTrace.inferStack) ++ (show $ e^.inferTrace.inferChildren) ++ (show e)
        tcEnvSet (inferTraceEnter (Syntax.getPos ast))
        (newAst, astType) <- doInferType ast
        --tcEnvSet (\env -> env & debugTypings %~ M.insert (Syntax.getPos ast) astType)
        tcEnvSet (inferTraceQuit (Syntax.getPos ast) astType)
        --liftPipelineTC $ printLogInfoStr $ "<- " ++ (show $ Syntax.getPos ast)
        return (newAst, astType)


withVar :: Position -> (TypeCheckerEnv -> Position -> (Type.Name, Type.Type) -> (Type.Name, Type.Type) -> Errors.Error) -> Type.Name -> Type.Type -> TypeChecker x -> TypeChecker x
withVar sourcePosition errorHandler name t m = do
    env <- tcEnv
    -- TODO: Add check for shadowing of "this"
    newEnv <- either (\(newName, prevName, prevType) -> failure $ errorHandler env sourcePosition (name, t) (prevName, prevType)) (return) (addVar name t env)
    withStateT (const newEnv) m


getFunction :: String -> TypeChecker (Maybe Type.Function)
getFunction name = tcEnvGet ((flip findFunction) name)

getVar :: String -> TypeChecker (Maybe Type.Type)
getVar name = (maybe (return Nothing) (return . Just . snd)) =<< (tcEnvGet $ lookupVar name)

internalTCFailure :: String -> Errors.InternalTCError -> TypeChecker a
internalTCFailure src err = do
    env <- tcEnv
    failure $ Errors.InternalTypecheckerFailure env src err

withSeparateScope :: Position -> TypeChecker x -> TypeChecker x
withSeparateScope pos m =
    withStateT revertScope . return =<< withStateT (separateScope pos) m

withClassContext :: String -> TypeChecker x -> TypeChecker x
withClassContext className m = do
    -- TODO: Add class this to env here!! (see classEnv)
    withStateT (\env -> env & currentClass .~ Nothing) . return =<< (\cls -> withStateT (\env -> env & currentClass %~ (\_ -> Just cls)) m) =<< maybe (internalTCFailure "withClassContext" $ Errors.ITCEClassContextNotAvailable $ Just className) return =<< findClass className

withMethodContext :: String -> TypeChecker x -> TypeChecker x
withMethodContext methodName m = do
        env <- tcEnv
        (Type.Class clsName@(Syntax.Ident pos currentClassName) _ _ currentClassDecl) <- maybe (internalTCFailure "withMethodContext" $ Errors.ITCEClassContextNotAvailable Nothing) (return) $ env^.currentClass
        member <- tcEnvGet ((flip findMember) methodName)
        case member of
                (Just (Type.Method methodName methodType args (Syntax.MethodDecl a t id as b))) -> do
                    withSeparateScope pos (methodContext clsName methodName methodType args (Syntax.FunctionDef a t id as b) m)
                _ -> internalTCFailure "withMethodContext" $ Errors.ITCEMissingClassMember currentClassName methodName
    where
        methodContext :: Syntax.Ident Position -> Type.Name -> Type.Type -> (M.Map String (Type.Name, Type.Type)) -> (Syntax.Definition Position) -> TypeChecker x -> TypeChecker x
        methodContext clsName methodName methodType args currentClassDecl m = do
            env <- tcEnv
            let declPos = Syntax.getPos currentClassDecl
            thisEnv <- either (\(n1, n2, t) -> internalTCFailure "withMethodContext" $ Errors.ITCEDuplicateMethodArg n1 n2 t) (return) $ addVar (Syntax.Ident declPos "this") (Syntax.ClassT declPos $ clsName) env
            newEnv <- foldM (\accEnv (varName, varType) -> either (\(n1, n2, t) -> internalTCFailure "withMethodContext" $ Errors.ITCEDuplicateMethodArg n1 n2 t) (return) $ addVar varName varType accEnv) thisEnv args
            withStateT (\env -> env & currentFunction .~ Nothing) . return =<< withStateT (const $ newEnv & currentFunction %~ (\_ -> Just $ (Type.Fun methodName methodType args currentClassDecl))) m 
 

withFunctionContext :: String -> TypeChecker x -> TypeChecker x
withFunctionContext funcName m = do
    fn@(Type.Fun (Syntax.Ident pos _) _ _ _) <- maybe (internalTCFailure "withFunctionContext" $ Errors.ITCEFunctionContextNotAvailable $ Just funcName) return =<< tcEnvGet (flip findFunction funcName)
    withSeparateScope pos (functContext fn m)
    where 
        functContext :: Type.Function -> TypeChecker x -> TypeChecker x
        functContext fn@(Type.Fun (Syntax.Ident pos _) _ args _) m = do
            env <- tcEnv
            newEnv <- foldM (\env (varName, varType) -> either (\(n1, n2, t) -> internalTCFailure "withFunctionContext" $ Errors.ITCEDuplicateFunctionArg n1 n2 t) (return) $ addVar varName varType env) env args
            withStateT (\env -> env & currentFunction .~ Nothing) . return =<< withStateT (const $ newEnv & currentFunction %~ (\_ -> Just fn)) m


getContextClassType :: TypeChecker (Maybe Type.Type)
getContextClassType =
    maybe (return Nothing) (\(Type.Class className@(Syntax.Ident pos _) _ _ _) -> return $ Just $ Syntax.ClassT pos className) =<< ((tcEnvGet $ (\env -> env^.currentClass)))

getContextFunction :: TypeChecker Type.Function
getContextFunction =
    maybe (internalTCFailure "getContextFunction" $ Errors.ITCEFunctionContextNotAvailable Nothing) (return) =<< ((tcEnvGet $ (\env -> env^.currentFunction)))

getContextFunctionReturnType :: TypeChecker Type.Type
getContextFunctionReturnType =
    maybe (internalTCFailure "getContextFunction" $ Errors.ITCEFunctionContextNotAvailable Nothing) (\(Type.Fun _ retType _ _) -> return retType) =<< ((tcEnvGet $ (\env -> env^.currentFunction)))

checkArgsRedeclaration :: (TypeCheckerEnv -> (Type.Function) -> (Syntax.Arg Position) -> [(Syntax.Arg Position)] -> Errors.Error) -> [Syntax.Arg Position] -> TypeChecker ()
checkArgsRedeclaration errorHandler args = 
    mapM_ (\elem@(Syntax.Arg _ _ (Syntax.Ident _ elemName)) -> checkError errorHandler elem $ filter (\other@(Syntax.Arg _ _ (Syntax.Ident _ otherName)) -> elemName == otherName && elem /= other) args) args
    where
        checkError :: (TypeCheckerEnv -> (Type.Function) -> (Syntax.Arg Position) -> [(Syntax.Arg Position)] -> Errors.Error) -> (Syntax.Arg Position) -> [(Syntax.Arg Position)] -> TypeChecker ()
        checkError errorHandler arg [] = return ()
        checkError errorHandler arg duplicates = failure =<< tcEnvGet (\env -> maybe (Errors.InternalTypecheckerFailure env "checkArgsRedeclaration" $ Errors.ITCEFunctionContextNotAvailable Nothing) (\fn -> errorHandler env fn arg duplicates) $ env^.currentFunction)

getMemberType :: Bool -> (Type.Class -> Bool) -> Syntax.Ident Position -> Syntax.Ident Position -> TypeChecker (Maybe (Type.Type))
getMemberType isOptional checkClass className@(Syntax.Ident _ classId) memberName@(Syntax.Ident _ memberId) = do
    env <- tcEnv
    cls <- maybe (failure $ Errors.UnknownType env className) return =<< findClass classId
    chain <- maybe (failure $ Errors.UnknownType env className) return $ findClassInheritanceChain env classId
    if not $ checkClass cls then failure $ Errors.UnknownClassMember env cls memberName chain else return ()
    maybe (if not isOptional then failure $ Errors.UnknownClassMember env cls memberName chain else return $ Nothing) (return . Just . Type.memberType) $ listToMaybe $ mapMaybe (Type.findClassMember memberId) chain



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
    
-- checkCastUp :: Position -> Syntax.Type Position -> Syntax.Type Position -> TypeChecker ()
-- checkCastUp pos tFrom tTo = do
--     c <- canBeCastUp tFrom tTo
--     if c then return ()
--     else todoImplementError $ "Cannot convert types " ++ (printi 0 tFrom) ++ " to " ++ (printi 0 tTo) --throw ("Cannot convert " ++ printi 0 tFrom ++ " to "++printi 0 tTo, pos)

typeFromArg :: Errors.TypeContext ->  Syntax.Arg Position -> TypeChecker (Type.Type, Syntax.Arg Position)
typeFromArg typeContext arg@(Syntax.Arg pos t id) = assureProperType typeContext t >> return (t, arg)

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

assureProperType :: Errors.TypeContext -> Syntax.Type Position -> TypeChecker ()
assureProperType typeContext invalidType@(Syntax.InfferedT pos) = do
    env <- tcEnv
    failure $ Errors.ImpossibleInference env typeContext invalidType pos
assureProperType _ _ = return ()

instance TypeCheckable Syntax.Program where
    doCheckTypes (Syntax.Program pos defs) = mapM checkTypes defs >>= return . Syntax.Program pos

instance TypeCheckable Syntax.Definition where
    doCheckTypes fn@(Syntax.FunctionDef pos tret id@(Syntax.Ident _ funName) args b) = do
        checkTypeExists Type.AllowVoid (Errors.TypeInFunctionReturn fn) tret
        mapM (\arg -> typeFromArg (Errors.TypeInFunctionArgDecl fn arg) arg) args >>= mapM_ (\(t, arg) -> checkTypeExists NoVoid (Errors.TypeInFunctionArgDecl fn arg) t)
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
        mapM (\arg -> typeFromArg (Errors.TypeInMethodArgDecl method arg) arg) args >>= mapM_ (\(t, arg) -> checkTypeExists NoVoid (Errors.TypeInMethodArgDecl method arg) t)
        checkedBody <- withMethodContext methodName (checkArgsRedeclaration (Errors.DuplicateFunctionArgument) args >> checkTypesFunctionBlock b)
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
                assureProperType (Errors.TypeInVarDecl declItem) t 
                checkTypeExists NoVoid (Errors.TypeInVarDecl declItem) t
                nds <- withVar srcPos Errors.VariableRedeclared id t (checkDecls srcPos ds)
                return (d:nds) -- TODO Handle env -> env? f . addVar id t
            checkDecls srcPos (d@(t, declItem@(Syntax.Init pos id e)):ds) = do
                --checkRedeclaration id
                env <- tcEnv
                (ne, et) <- inferType e
                (nt, nne) <- case t of
                        Syntax.InfferedT _ -> case et of
                                        invalidType@(Syntax.InfferedT _) -> failure $ Errors.ImpossibleInference env (Errors.TypeInVarDecl declItem) invalidType pos
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
        liftPipelineTC $ printLogInfo $ T.pack $ "Assignemnt mapped LEFT ?=> " ++ (printi 0 nase)
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
        checkTypeExists Type.NoVoid (Errors.TypeInReturn stmt) rt 
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
    doCheckTypes ifelse@(Syntax.IfElse pos econd strue sfalse) = do
        (necond, econdt) <- inferType econd
        env <- tcEnv
        case econdt of
            Syntax.BoolT _ -> case strue of
                        Syntax.VarDecl pv _ -> failure $ Errors.ConditionSingleVarDeclaration env ifelse $ Errors.IfTrueBranch strue
                        _ -> do
                            nst <- checkTypes strue
                            case sfalse of
                                Syntax.VarDecl pv _ -> failure $ Errors.ConditionSingleVarDeclaration env ifelse $ Errors.IfTrueBranch sfalse
                                _ -> do
                                    nsf <- checkTypes sfalse
                                    return $ Syntax.IfElse pos necond nst nsf
            _ -> failure $ Errors.ConditionNonLogicalValue env ifelse econdt $ Errors.IfConditionPredicate necond
    doCheckTypes while@(Syntax.While pos econd stmt) = do
        (necond, econdt) <- inferType econd
        env <- tcEnv
        case econdt of
            Syntax.BoolT _ -> case stmt of
                        Syntax.VarDecl pv _ -> failure $ Errors.ConditionSingleVarDeclaration env while $ Errors.WhileBodyBlock stmt
                        _ -> do
                            nst <- checkTypes stmt
                            return $ Syntax.While pos necond nst
            _ -> failure $ Errors.ConditionNonLogicalValue env while econdt $ Errors.WhileConditionPredicate necond
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
        else do
            env <- tcEnv
            failure $ Errors.NumericConstantExceedsTypeLimit env l i [("<256 >=0", Syntax.ByteT pos), ("<2^31 >=-2^31", Syntax.IntT pos)]
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
                        mm <- getMemberType True (\_ -> True) idc id
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
                failure $ Errors.UnknownVariable env id 
            elemF i@(Syntax.Ident _ name) (f@(Type.Fun (Syntax.Ident _ n) _ _ _):xs) =
                if name == n then Just f
                else elemF i xs
            elemF _ [] = Nothing
    doInferType appCall@(Syntax.App pos efun es) = do
        (nef, eft) <- inferType efun
        case eft of
            fn@(Syntax.FunT _ ret args) -> do
                nes <- mapM inferType es
                let efts = map snd nes
                env <- tcEnv
                if length efts /= length args then
                    failure $ Errors.CallIncompatibleNumberOfParameters env nef fn nes
                else return ()
                mapM_ (\(index,(l,(e,r))) -> checkCastUpErr (\env -> Errors.CallInvalidParameterType env appCall fn index) (e ^. position @1) r l) $ zip [1..] $ zip args nes
                return (Syntax.App pos nef (map fst nes), ret)
            _ -> do
                env <- tcEnv
                nes <- mapM inferType es
                failure $ Errors.CallNotCallableType env nef eft nes
    doInferType cast@(Syntax.Cast pos t e) = do
        checkTypeExists Type.NoVoid (Errors.TypeInCast cast) t
        env <- tcEnv
        case t of
            Syntax.InfferedT _ -> failure $ Errors.IllegalTypeUsed env (Errors.TypeInCast cast) t
            _ -> do
                (ne, et) <- inferType e
                c <- canBeCastDown et t
                if c then
                    case ne of
                        (Syntax.Cast _ _ ie) -> return (Syntax.Cast pos t ie, t)
                        (Syntax.Lit _ (Syntax.Null _)) -> return (ne, t)
                        _ -> return (Syntax.Cast pos t ne, t)
                else failure $ Errors.IncompatibleTypesCast env cast ne et t
    doInferType stmt@(Syntax.ArrAccess pos earr ein _) = do
        (nearr, art) <- inferType earr
        env <- tcEnv
        case art of
            Syntax.ArrayT _ t -> do
                (nein, et) <- inferType ein
                case et of
                    Syntax.IntT _ -> return (Syntax.ArrAccess pos nearr nein (Just t), t)
                    Syntax.ByteT _ -> return (Syntax.ArrAccess pos nearr (Syntax.Cast pos int nein) (Just t), t)
                    _ -> failure $ Errors.ArrayAccessNonNumericIndex env et nein stmt
            _ -> failure $ Errors.IndexAccessNonCompatibleType env art nearr stmt
    doInferType stmt@(Syntax.NewObj pos t m) = do
        checkTypeExists NoVoid (Errors.TypeInNew stmt) t
        env <- tcEnv
        case m of
            Nothing -> do
                case t of
                    Syntax.ClassT _ (Syntax.Ident _ n) -> if n /= "String" then return ()
                    else failure $ Errors.NewUsageOnString env stmt
                    _ -> failure $ Errors.NewUsageOnNonClass env t stmt
                return (Syntax.NewObj pos t m, t)
            Just e -> do
                (ne, et) <- inferType e
                b <- canBeCastUp et int
                if b then return (Syntax.NewObj pos t (Just ne), Syntax.ArrayT pos t)
                else failure $ Errors.NewArrayNonNumericDimensions env et ne stmt
    doInferType stmt@(Syntax.Member pos e id _) = do
        (ne, et) <- inferType e
        env <- tcEnv
        case et of
            Syntax.StringT _ -> getInnerMemberType pos ne id (name "String")
            Syntax.ArrayT _ _ -> getInnerMemberType pos ne id (name "Array")
            Syntax.ClassT _ name -> getInnerMemberType pos ne id name
            Syntax.InfferedT _ -> getInnerMemberType pos ne id (name "Object")
            _ -> failure $ Errors.FieldAccessNonCompatibleType env et ne stmt
        where
            getInnerMemberType pos e id@(Syntax.Ident p i) cls@(Syntax.Ident _ clsName) = do
                env <- tcEnv
                mem <- getMemberType False (\_ -> not $ clsName == "Array" && (i == "elements" || i == "elementSize")) cls id
                case mem of
                    Just t -> return (Syntax.Member pos e id (Just clsName), t)
                    Nothing -> failure $ Errors.InternalTypecheckerFailure env "getInnerMemberType" $ Errors.ITCEMissingMember clsName i
    doInferType (Syntax.UnaryOp pos op e) = do
        (ne, et) <- inferType e
        env <- tcEnv
        case (op, et) of
            (Syntax.Not _, Syntax.BoolT _) -> return (Syntax.UnaryOp pos op ne, et)
            (Syntax.Neg _, Syntax.IntT _) -> return (Syntax.UnaryOp pos op ne, et)
            (Syntax.Neg _, Syntax.ByteT _) -> return (Syntax.UnaryOp pos op ne, et)
            (_, _) -> failure $ Errors.IncompatibleTypesUnaryOp env op (ne, et)
    doInferType (Syntax.BinaryOp pos op el er) = do
        (nel, elt) <- inferType el
        (ner, ert) <- inferType er
        env <- tcEnv
        let err = failure $ Errors.IncompatibleTypesBinaryOp env op (nel, elt) (ner, ert)
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