{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Linearized.Converter where

import qualified Typings.Types as Types
import qualified Program.Syntax as A
import qualified Linearized.Syntax as B

import qualified Utils.Containers.IDMap as IM
import qualified Data.Map as M
import Data.List

import Control.Lens hiding(transform)

import Reporting.Errors.Position
import qualified Reporting.Errors.Def as Errors
import Reporting.Logs
import Linearized.Syntax (IRPosition(..))
import Linearized.Env
import Linearized.Def

import Linearized.BuiltIns(builtIns)

import qualified Data.Text as T

import qualified Typings.Env as TypeChecker
import qualified Typings.Types as Types

import Data.Maybe
import Data.Bifunctor

import Control.Monad

unknownType = B.Reference Undefined $ B.Label Undefined "?"

class  (A.IsSyntax ma Position, B.IsIR mb) => IRConvertable ma mb c | ma -> mb, ma -> c where
    doTransform :: B.Type Position -> ma Position -> LinearConverter () (c, [mb Position])
    transformOver :: [ma Position] -> LinearConverter () (c, [mb Position])
    transformOver = return . (\(c, ast) -> (head c, concat ast)) . unzip <=< mapM (transform $ B.Reference Undefined $ B.Label Undefined "?")

    transformOverOnly :: [ma Position] -> LinearConverter () [mb Position]
    transformOverOnly = return . snd <=< transformOver

    transformOnly :: B.Type Position -> ma Position -> LinearConverter () [mb Position]
    transformOnly expectedType = return . snd <=< (transform expectedType)

    transform :: B.Type Position -> ma Position -> LinearConverter () (c, [mb Position])
    transform expectedType ast = do
        --liftPipelineOpt $ printLogInfo $ T.pack $ "Transform to IR: " ++ (show ast)
        r <- doTransform expectedType ast
        --liftPipelineOpt $ printLogInfo $ T.pack $ "[DONE] Transform to IR: " ++ (show ast)
        return r
    -- return . B.modifyPos (\_ -> posFrom ast)  =<< 

justEmit :: [mb Position] -> LinearConverter () ((), [mb Position])
justEmit = return . (,) ()

noEmit :: LinearConverter () ((), [mb Position])
noEmit = justEmit []

-- stripClassName :: String -> String
-- stripClassName pm = case findIndex (== '_') (drop 1 pm) of
--     Just i -> drop (i+2) pm

data FnProto = FnProto Position (Maybe (B.Label Position)) (B.Label Position) (B.Type Position) [A.Arg Position] [A.Stmt Position]

newNameFor :: A.Ident Position -> B.Type Position -> LinearConverter () (B.Name Position)
newNameFor (A.Ident pos n) t = do
    (B.Name _ n') <- newName t
    lcStateSet (\env -> env & varMap %~ M.insert n n')
    return $ B.Name pos n'

newName :: B.Type Position -> LinearConverter () (B.Name Position)
newName t = do
    i <- lcStateGet (^. varNameCounter)
    let n = "t_"++show i
    lcStateSet (\env -> env & varNameCounter %~ (+1) & varType %~ M.insert n t)
    return $ B.Name (B.getPos t) n

newLabel :: String -> LinearConverter () (B.Label Position)
newLabel prefix = do
    i <- lcStateGet (^. varNameCounter)
    lcStateSet (\env -> env & varNameCounter %~ (+1))
    return (B.Label Undefined $ prefix++show i)

nameOf :: Position -> String -> LinearConverter () (B.Name Position)
nameOf pos varName =
    maybe (failure (\(tcEnv, lnEnv) -> Errors.InternalLinearizerFailure tcEnv lnEnv "nameOf" $ Errors.ILNEMissingVariable varName)) (return . (B.Name pos)) . M.lookup varName =<< lcStateGet (^.varMap)

typeOf :: (B.Name Position) -> LinearConverter () (B.Type Position)
typeOf (B.Name _ varName) =
    maybe (failure (\(tcEnv, lnEnv) -> Errors.InternalLinearizerFailure tcEnv lnEnv "typeOf" $ Errors.ILNEMissingVariable varName)) return . M.lookup varName =<< lcStateGet (^.varType)


ct :: (Show a) => A.Type a -> B.Type a
ct (A.BoolT p) = B.ByteT p
ct (A.ByteT p) = B.ByteT p
ct (A.VoidT p) = B.ByteT p
ct (A.IntT p) = B.IntT p
ct (A.ArrayT p t) = B.ArrT p $ ct t
ct (A.StringT p) = B.Reference p $ B.Label p "String"
ct (A.ClassT p (A.Ident _ clname)) = B.Reference p $ B.Label p clname
-- FIXME: ct handle inferredt and funt?

getMethod :: Position -> String -> String -> LinearConverter () (B.Method Position)
getMethod pos clsName methodName = do
    --cls <- maybe (failure (\(tcEnv, lnEnv) -> Errors.InternalLinearizerFailure tcEnv lnEnv "getMethodInfo" $ Errors.ILNEMissingClass name Nothing pos)) return $ TypeChecker.findClassInEnv tcEnv name
    struct@(B.Struct _ _ _ methods _) <- join $ lcStateGet (\env -> IM.findM (idMapFailure "getMethod" (\clsName -> Errors.ILNEMissingClass (clsName) Nothing pos)) (clsName) (env ^. structures))
    --existingMethodName <- maybe (failure (\(tcEnv, lnEnv) -> Errors.InternalLinearizerFailure tcEnv lnEnv "getMethod" $ Errors.ILNEMissingMethod methodName struct)) (return) $ listToMaybe $ filter (\name -> stripClassName name == methodName) $ IM.mapList (\name _ -> name) methods
    existingMethodName <- maybe (failure (\(tcEnv, lnEnv) -> Errors.InternalLinearizerFailure tcEnv lnEnv "getMethod" $ Errors.ILNEMissingMethod methodName struct)) (return . fst) $ listToMaybe $ filter snd $ IM.mapList (\k (B.Method _ (B.Label _ mCls) (B.Label _ mName) _ _) -> (k, mCls == clsName && mName == methodName)) methods
    (_, method, methodIndex) <- IM.findElemM (idMapFailure "getMethod" (`Errors.ILNEMissingMethod` struct)) (existingMethodName) methods
    return method

getField :: Position -> String -> String -> LinearConverter () (B.Field Position)
getField pos clsName fieldName = do
    --liftPipelineOpt $ printLogInfoStr $ "get field all structurs are " ++ (show pppp)
    --cls <- maybe (failure (\(tcEnv, lnEnv) -> Errors.InternalLinearizerFailure tcEnv lnEnv "getMethodInfo" $ Errors.ILNEMissingClass name Nothing pos)) return $ TypeChecker.findClassInEnv tcEnv name
    struct@(B.Struct _ _ _ _ fields) <- join $ lcStateGet (\env -> IM.findM (idMapFailure "getField" (\clsName -> Errors.ILNEMissingClass (clsName) Nothing pos)) (clsName) (env ^. structures))
    IM.findM (idMapFailure "getField" (`Errors.ILNEMissingMethod` struct)) fieldName fields

-- getFieldOffset :: Position -> String -> String -> LinearConverter () B.Offset
-- getFieldOffset pos clsName = (\(_, _, o) -> return o) <=< getField pos clsName

getFieldType :: Position -> String -> String -> LinearConverter () (B.Type Position)
getFieldType pos clsName = (\(B.Field _ t _) -> return t) <=< getField pos clsName

getFunctionType :: B.Label Position -> LinearConverter () (B.Type Position)
getFunctionType (B.Label pos fnName) =
    (\(B.Fun _ _ _ t _ _) -> return t) =<< join (lcStateGet (\env -> IM.findM (idMapFailure "getFunctionType" Errors.ILNEUndefinedFunction) fnName (env ^. functions)))

getFunctionArgsTypes :: B.Label Position -> LinearConverter () [B.Type Position]
getFunctionArgsTypes (B.Label pos fnName) =
    (\(B.Fun _ _ _ _ argst _) -> return $ map fst argst) =<< join (lcStateGet (\env -> IM.findM (idMapFailure "getFunctionArgsTypes" Errors.ILNEUndefinedFunction) fnName (env ^. functions)))


collectStructures :: [Types.Class] -> LinearConverter () [B.Structure Position]
collectStructures classes = do
    structs <- IM.fromM (idMapFailure "collectStructures" Errors.ILNEDuplicateStructure) . concat =<< mapM collectFromClassDef classes
    lcStateSet (\env -> env & structures .~ structs)
    lcStateGet (\env -> IM.elems (env ^. structures))
    where
        collectFromClassDef :: Types.Class -> LinearConverter () [B.Structure Position]
        collectFromClassDef c@(Types.Class _ _ _ (A.ClassDef _ (A.Ident BuiltIn "") _ _)) = do
            -- Must use the typings instead
            collectFromClass c
        collectFromClassDef (Types.Class _ _ _ def) = collectFromDef def
        collectFromDef :: A.Definition Position -> LinearConverter () [B.Structure Position]
        collectFromDef def@(A.ClassDef pos (A.Ident _ name) _ _) = do
            tcEnv <- lcStateGet (^.typings)
            cls <- maybe (failure (\(tcEnv, lnEnv) -> Errors.InternalLinearizerFailure tcEnv lnEnv "collectFromDef" $ Errors.ILNEMissingClass name (Just def) pos)) return $ TypeChecker.findClassInEnv tcEnv name
            collectFromClass cls
        collectFromDef _ = return []
        collectFromClass :: Types.Class -> LinearConverter () [B.Structure Position]
        collectFromClass def@(Types.Class (A.Ident clnamePos clname) parent members _) = do
            let newName = B.Label clnamePos clname
            tcEnv <- lcStateGet (^.typings)
            chain <- maybe (failure (\(tcEnv, lnEnv) -> Errors.InternalLinearizerFailure tcEnv lnEnv "collectStructures" $ Errors.ILNEMissingClassDefinition clname def)) return $ TypeChecker.findClassInheritanceChain tcEnv clname
            selfMethods <- IM.fromM (idMapFailure "collectFromClass" $ Errors.ILNEEncounteredDuplicateStructureMember newName) $ mapMaybe (translateToMethod clname) members
            selfFields <- IM.fromM (idMapFailure "collectFromClass" $ Errors.ILNEEncounteredDuplicateStructureMember newName) $ mapMaybe (translateToField clname) members
            let parentChain = reverse $ tail chain
            let newParent = case parent of
                    (A.NoName _) -> Nothing
                    (A.Name _ (A.Ident idpos id)) -> Just $ B.Label idpos id
            let structPrototype = B.Struct clnamePos newName [] IM.empty IM.empty
            chainMembers <- concat <$> mapM (return . map (\(B.Struct _ (B.Label _ name) _ methods fields) -> (name, fields, methods)) <=< collectFromClass) parentChain
            completeStruct <- foldM (\struct (name, fields, methods) -> classParentMerge clname name struct fields methods) structPrototype chainMembers
            completeStruct' <- classParentMerge clname clname completeStruct selfFields selfMethods
            return [completeStruct']
        translateToMethod :: String -> Types.Member -> Maybe (B.Method Position)
        translateToMethod clname (Types.Method (A.Ident p n) retType args _) = Just $ B.Method p (B.Label p clname) (B.Label p n) (ct retType) (map (\(A.Ident p' argName, argType) -> (ct argType, B.Name p' argName)) $ M.elems args)--(B.Label p $ "_" ++ clname ++ "_" ++ n)
        translateToMethod _ _ = Nothing
        translateToField :: String -> Types.Member -> Maybe (B.Field Position)
        translateToField _ (Types.Field (A.Ident p n) t _) = Just $ B.Field p (ct t) (B.Label p n)
        translateToField _ _ = Nothing
        classParentMerge :: String -> String -> B.Structure Position -> IM.Map (B.Field Position) -> IM.Map (B.Method Position) -> LinearConverter () (B.Structure Position)
        classParentMerge clsName parentName (B.Struct pos name chain methods fields) parentFields parentMethods = do
            -- (IM.mapList (\_ (B.Label lPos id) -> B.Label pos $ "_"++clsName++"_z"++stripClassName id) parentMethods)
            --let overridenMethods = filter (\name -> let pkeys = IM.keys parentMethods in any (\pname -> stripClassName name == stripClassName pname) pkeys) $ IM.keys methods
            let parentMethodEntries = map fst $ filter snd $ IM.mapList (\k m@(B.Method _ (B.Label _ parName) name _ _) -> ((m, name), parName == parentName)) parentMethods
            let overridenMethods = map fst $ filter snd $ IM.mapList (\k m@(B.Method _ _ name _ _) -> ((k, m), any (\(_, pname) -> pname == name) parentMethodEntries)) methods
            --let newMethods = (IM.mapList (\_ (B.Label lPos id) -> B.Label pos $ "_"++parentName++"_"++stripClassName id) parentMethods)
            let newMethods = map (\(B.Method mPos mClass mName mType mArgs) -> B.Method mPos (B.Label pos parentName) mName mType mArgs) $ map fst parentMethodEntries
            combinedMethods <- IM.insertManyM (idMapFailure "classParentMerge" $ Errors.ILNEEncounteredDuplicateStructureMember name) newMethods $ IM.deleteMany (map fst overridenMethods) methods
            --combinedMethods <- IM.insertManyM (idMapFailure "classParentMerge" $ Errors.ILNEEncounteredDuplicateStructureMember name) (IM.mapList (\_ (B.Label lPos id) -> B.Label pos $ "_"++clsName++"_z"++stripClassName id) parentMethods) methods
            combinedFields <- IM.concatSequenceM (idMapFailure "classParentMerge" $ Errors.ILNEEncounteredDuplicateStructureMember name) (\m field -> field) parentFields fields
            --(B.Label idpos $ "_class_"++id) name newParent
            --newParent <- return $ parent >>= (\(A.Label _ pid) -> return $ w"_class_"++pid)
            --IM.concatM (idMapFailure "classParentMerge" $ Errors.ILNEEncounteredDuplicateStructureField name) fields ()
            return $ B.Struct pos name (B.Label pos parentName : chain) combinedMethods combinedFields 
        measureOffset :: IM.Map (B.Label a, B.Type a, B.Offset) -> B.Size
        measureOffset m = case IM.last m of
            Nothing -> 0
            (Just (_, B.IntT _, o)) -> o + 0x04
            (Just (_, B.ByteT _, o)) -> o + 0x01
            (Just (_, B.Reference _ _, o)) -> o + 0x08

-- collectFn (A.FunctionDef _ t (A.Ident _ name) args block) = do
--             if elem name (map (\(B.Fun l _ _ _) ->l) funcs) then
--                 return []--transF name args block
--             else return [toEntity $ B.Fun name (ct t) [] []]
collectFunctions :: [A.Definition Position] -> LinearConverter () [B.Function Position]
collectFunctions defs = do
    let fns = concatMap collectFromDef defs
    fnPrototypes <- IM.fromM (idMapFailure "collectFunctions" Errors.ILNEDuplicateFunctionName) $ builtIns ++ map (\(FnProto pos cls name@(B.Label _ id) retType _ _) -> B.Fun pos cls name retType [] []) fns
    lcStateSet (\env -> env & functions .~ fnPrototypes)
    --liftPipelineOpt $ printLogInfo $ T.pack $ show fnPrototypes
    mapM_ (transformFunction >=> overrideFunction) fns
    lcStateGet (\env -> IM.elems (env ^. functions) \\ builtIns)
    where
        collectFromDef :: A.Definition Position -> [FnProto]
        collectFromDef (A.FunctionDef pos t (A.Ident idpos name) args (A.Block _ stmts)) = [FnProto pos Nothing (B.Label idpos name) (ct t) args stmts]
        collectFromDef (A.ClassDef _ (A.Ident _ clname) _ mems) = concatMap (collectFromClassDecl clname) mems
        collectFromClassDecl :: String -> A.ClassDecl Position -> [FnProto]
        collectFromClassDecl _ (A.FieldDecl {}) = []
        collectFromClassDecl clname (A.MethodDecl pos t (A.Ident idpos n) args  (A.Block _ stmts)) =
            [FnProto pos (Just $ B.Label idpos clname) (B.Label idpos n) (ct t) (A.Arg Undefined (A.ClassT Undefined (A.Ident Undefined clname)) (A.Ident Undefined "this") : args) stmts]
transformFunction :: FnProto -> LinearConverter () (B.Function Position)
transformFunction (FnProto pos cls name@(B.Label _ id) retType args stmts) = do
    nargs <- mapM (\(A.Arg _ t n) -> newNameFor n (ct t) <&> (,) (ct t)) args
    nstmts <- withLCState (\env -> env & returnContextType .~ Nothing) . return =<< (withLCState (\env -> env & returnContextType .~ (Just retType)) (transformOverOnly stmts))
    return $ B.Fun pos cls name retType nargs nstmts

transformProgram :: (A.Program Position) -> LinearConverter () (B.Program Position)
transformProgram prog = do
    nprogs <- doTransformProg prog
    let (poss, structs, fns, datas) = unzip4 $ map (\(B.Program pos structs fns datas) -> (pos, structs, fns, datas)) nprogs
    structsMap <- IM.concatMapsM (idMapFailure "transformProgram" Errors.ILNEDuplicateStructure) structs
    fnsMap <- IM.concatMapsM (idMapFailure "transformProgram" Errors.ILNEDuplicateFunctionName) fns
    datasMap <- IM.concatMapsM (idMapFailure "transformProgram" Errors.ILNEDuplicateLabelledData) datas
    return $ B.Program (head poss) structsMap fnsMap datasMap
    where
        doTransformProg ::(A.Program Position) -> LinearConverter () [B.Program Position]
        doTransformProg (A.Program a tds) = do
            classes <- lcStateGet (TypeChecker.allClasses . (^.TypeChecker.definedClasses) . (^.typings))
            structs <- IM.fromM (idMapFailure "transformProgram" Errors.ILNEDuplicateStructure) =<<  collectStructures classes
            fns <- IM.fromM (idMapFailure "transformProgram" Errors.ILNEDuplicateFunctionName) =<< collectFunctions tds
            datas <- lcStateGet (^. datas)
            return [B.Program a structs fns datas]

instance IRConvertable A.Expr B.Stmt (B.Name Position) where
    doTransform _ (A.Lit pos (A.String _ s)) = do
        (B.DataString _ _ strLabel, newDatas) <- join $ lcStateGet (\env -> IM.provideM (\_ -> (\l -> B.DataString pos s l) <$> newLabel "_S") s $ env ^. datas)
        lcStateSet (\env -> env & datas .~ newDatas)
        n <- newName $ B.Reference pos $ B.Label pos "String"
        return (n, [B.VarDecl pos (B.Reference pos $ B.Label pos "String") n (B.NewString pos strLabel)])
    doTransform et (A.Lit p l) = do
        n <- newName (litType et l)
        let c = litC et l
        return (n, [B.VarDecl p (litType et l) n (B.Val p (B.Const p c))])
        where
            litType :: (B.Type Position) -> (A.Lit Position) -> (B.Type Position)
            litType et l = case l of 
                            A.Null p -> et
                            A.Int p _ -> B.IntT p
                            A.Byte p _ -> B.ByteT p
                            A.Bool p _ -> B.ByteT p
            litC :: (B.Type Position) ->  (A.Lit Position) -> (B.Constant Position)
            litC et l = case l of                    
                        A.Null p -> B.Null p et
                        A.Int p i -> B.IntC p i
                        A.Byte p i -> B.ByteC p i
                        A.Bool p True -> B.ByteC p 1
                        A.Bool p False -> B.ByteC p 0
    doTransform _ (A.Var p (A.Ident _ x)) = ((flip (,)) []) <$> nameOf p x
    doTransform _ (A.Member p e (A.Ident _ field) (Just className)) = do
        (enm, enmc) <- transform (B.Reference p $ B.Label p className) e
        t <- getFieldType p className field
        n <- newName t
        return (n, enmc ++ [B.VarDecl p t n (B.MemberAccess p enm (B.Label p className) (B.Label p field) t)])
    doTransform _ (A.NewObj p t m) = 
        case m of
            Nothing -> do
                let (A.ClassT _ (A.Ident _ cls)) = t
                n <- newName (ct t)
                return (n, [B.VarDecl p (ct t) n (B.NewObj p $ B.Label p $ cls)])
            Just e -> do
                (en, enc) <- transform (ct t) e
                ten <- typeOf en
                let tt = ct t
                n <- newName $ B.ArrT p $ ct t
                return (n, enc ++ [B.VarDecl p (B.ArrT p $ ct t) n (B.NewArray p tt (B.Var p en ten))])
    doTransform _ (A.ArrAccess p el er (Just t)) = do
        (enl, enlc) <- transform (B.ArrT p $ ct t) el
        (enr, enrc) <- transform (B.IntT p) er
        tenr <- typeOf enr
        n <- newName (ct t)
        return (n, enlc ++ enrc ++ [B.VarDecl p (ct t) n (B.ArrAccess p enl (B.Var p enr tenr))])
    doTransform _ (A.Cast p t e) = do
        (en, enc) <- transform (ct t) e
        ent <- typeOf en
        case (ct t, ent) of
            (B.ByteT _, B.ByteT _) -> return (en, enc)
            (B.IntT _, B.IntT _) -> return (en, enc)
            (B.Reference _ t1, B.Reference _ t2) -> 
                case t of
                    A.ClassT p (A.Ident _ clsName) -> do
                        n <- newName (B.Reference p $ B.Label p clsName)
                        ten <- typeOf en
                        return (n, enc ++ [B.VarDecl p (B.Reference p t1) n (B.Cast p (B.Label p $ clsName) (B.Var p en ten))])
            (B.ByteT p', B.IntT _) -> do
                n <- newName $ B.ByteT p'
                ten <- typeOf en
                return (n, enc ++ [B.VarDecl p (B.ByteT p) n (B.IntToByte p (B.Var p en ten))])
            (B.IntT p', B.ByteT _) -> do
                n <- newName $ B.IntT p'
                ten <- typeOf en
                return (n, enc ++ [B.VarDecl p (B.IntT p) n (B.ByteToInt p (B.Var p en ten))])
    doTransform _ (A.UnaryOp _ op e) = do
        (en, enc) <- transform unknownType e
        ent <- typeOf en
        case op of
            A.Neg p -> do
                n <- newName ent
                let zero = case ent of {(B.IntT p) -> (B.IntC p 0); (B.ByteT p) -> (B.ByteC p 0)}
                ten <- typeOf en
                return (n, enc ++ [B.VarDecl p ent n (B.BinOp p (B.Sub p) (B.Const p zero) (B.Var p en ten))])
            A.Not p -> do
                n <- newName ent
                ten <- typeOf en
                return (n, enc ++ [B.VarDecl p ent n (B.Not p (B.Var p en ten))])
    doTransform _ (A.BinaryOp p (A.Add p2) (A.UnaryOp _ (A.Neg _) el) er) = transform unknownType (A.BinaryOp p (A.Sub p2) er el)
    doTransform _ (A.BinaryOp p (A.Add p2) el (A.UnaryOp _ (A.Neg _) er)) = transform unknownType (A.BinaryOp p (A.Sub p2) el er)
    doTransform _ e@(A.BinaryOp _ op el er) = 
        case op of
            A.Lt _ -> compare e
            A.Le _ -> compare e
            A.Equ _ -> compare e
            A.Neq _ -> compare e
            A.Gt _ -> compare e
            A.Ge _ -> compare e
            A.And _ -> compare e
            A.Or _ -> compare e
            innerOp -> do
                let p = A.getPos innerOp
                (enl, enlc) <- transform unknownType el
                (enr, enrc) <- transform unknownType er
                ent <- typeOf enl
                n <- newName ent
                let (bop, isReversed) = case op of
                            A.Add p -> (B.Add p, False)
                            A.Sub p -> (B.Sub p, True)
                            A.Mul p -> (B.Mul p, False)
                            A.Div p -> (B.Div p, True)
                            A.Mod p -> (B.Mod p, True)
                if isLit el && not isReversed then
                    return (n, enlc ++ enrc ++ [B.VarDecl p ent n (B.BinOp p bop (B.Var p enr ent) (B.Var p enl ent))])
                else
                    return (n, enlc ++ enrc ++ [B.VarDecl p ent n (B.BinOp p bop (B.Var p enl ent) (B.Var p enr ent))])
        where
            compare :: (A.Expr Position) ->  LinearConverter () (B.Name Position, [B.Stmt Position])
            compare e = do
                let p = A.getPos e
                nb <- newName $ B.ByteT p
                ltrue <- newLabel "_C"
                lfalse <- newLabel "_C"
                condc <- transformCondition e ltrue lfalse False
                return (nb, [B.VarDecl p (B.ByteT p) nb (B.Val p (B.Const p (B.ByteC p 0)))] ++ condc ++ [B.SetLabel p ltrue, B.Assign p (B.ByteT p) (B.Variable p nb) (B.Val p (B.Const p (B.ByteC p 1))), B.SetLabel p lfalse])
            isLit :: (A.Expr Position) -> Bool
            isLit (A.Lit _ _) = True
            isLit _ = False
    doTransform _ (A.App _ el es) = do
        case el of
            (A.Var p (A.Ident p' f)) -> do
                let f' = B.Label p' f
                t <- getFunctionType f'
                argst <- getFunctionArgsTypes f'
                liftPipelineOpt $ printLogInfo $ T.pack $ "APPDET " ++ show argst
                (ens, ensc) <- second concat <$> unzip <$> mapM (uncurry transform) (zip argst es)
                n <- newName t
                return (n, ensc ++ [B.VarDecl p t n (B.Call p f' (map (uncurry (B.Var p)) $ zip ens argst))])
            (A.Member p e (A.Ident _ m) (Just clsName)) -> do
                (en, enc) <- transform (B.Reference p $ B.Label p clsName) e
                ent <- typeOf en
                method@(B.Method _ _ _ retType args) <- getMethod p clsName m
                let argst = map fst args
                (ens, ensc) <- second concat <$> unzip <$> mapM (uncurry transform) (zip argst es)
                n <- newName retType
                ens' <- return $ en:ens
                return (n, ensc ++ enc ++ [B.VarDecl p retType n (B.MCall p en (B.Label p m) (B.Label p clsName) (map (uncurry (B.Var p)) $ zip ens' (ent:argst) ))])

opC :: A.BinOp Position -> B.Cmp Position
opC (A.Equ p) = B.Eq p
opC (A.Neq p) = B.Ne p
opC (A.Le p) = B.Le p
opC (A.Lt p) = B.Lt p
opC (A.Ge p) = B.Ge p
opC (A.Gt p) = B.Gt p

opNeg :: B.Cmp Position -> B.Cmp Position
opNeg (B.Eq p) = B.Ne p
opNeg (B.Ne p) = B.Eq p
opNeg (B.Le p) = B.Gt p
opNeg (B.Lt p) = B.Ge p
opNeg (B.Ge p) = B.Lt p
opNeg (B.Gt p) = B.Le p


transformCondition :: (A.Expr Position) -> (B.Label Position) -> (B.Label Position) -> Bool -> LinearConverter () [B.Stmt Position]
transformCondition ast@(A.UnaryOp p (A.Not _) e) ltrue lfalse neg = do
    liftPipelineOpt $ printLogInfo $ T.pack $ "transform condition: " ++ (show ast)
    transformCondition e lfalse ltrue (not neg)
transformCondition ast@(A.BinaryOp p op el er) ltrue lfalse neg =
    if A.isAA op then do
        liftPipelineOpt $ printLogInfo $ T.pack $ "transform condition: " ++ (show ast)
        (nl, nlc) <- transform unknownType el
        (nr, nrc) <- transform unknownType er
        nlt <- typeOf nl
        nrt <- typeOf nr
        case neg of
            False -> return $ nlc ++ nrc ++ [B.JumpCmp p (opNeg (opC op)) lfalse (B.Var p nl nlt) (B.Var p nr nrt)]
            True -> return $ nlc ++ nrc ++ [B.JumpCmp p (opC op) ltrue (B.Var p nl nlt) (B.Var p nr nrt)]
    else case op of
        (A.And _) -> do
            liftPipelineOpt $ printLogInfo $ T.pack $ "transform condition: " ++ (show ast)
            elc <- transformCondition el ltrue lfalse False
            erc <- transformCondition er ltrue lfalse neg
            return $ elc ++ erc
        (A.Or p) -> do
            liftPipelineOpt $ printLogInfo $ T.pack $ "transform condition: " ++ (show ast)
            lnext <- newLabel "_COR"
            elc <- transformCondition el ltrue lnext True
            erc <- transformCondition er ltrue lfalse neg
            return $ elc ++ [B.SetLabel p lnext] ++ erc
transformCondition e ltrue lfalse neg = do
    case e of
        A.Lit p (A.Bool _ True) ->
            case neg of
                False -> return []
                True -> return [B.Jump p ltrue]
        A.Lit p (A.Bool _ False) ->
            case neg of
                False -> return [B.Jump p lfalse]
                True -> return []
        _ -> do
            let p = A.getPos e
            (n, ec) <- transform unknownType e
            nt <- typeOf n
            case neg of
                False -> return $ ec ++ [B.JumpCmp p (B.Eq p) lfalse (B.Var p n nt) (B.Const p $ B.ByteC p 0)]
                True -> return $ ec ++ [B.JumpCmp p (B.Ne p) ltrue (B.Var p n nt) (B.Const p $ B.ByteC p 0)]

instance IRConvertable A.Stmt B.Stmt () where
    --doTransform ast = return [B.Return $ A.getPos ast]

    doTransform _ (A.Empty _) = noEmit
    doTransform _ (A.VarDecl _ decls) = justEmit =<< concat <$> mapM transformVarDecl decls
        where
            transformVarDecl :: (A.Type Position, A.DeclItem Position) -> LinearConverter () [B.Stmt Position]
            transformVarDecl (t, A.NoInit pos name) = do
                n <- newNameFor name (ct t)
                return $ [B.VarDecl pos (ct t) n (B.Val pos (B.Const pos (B.Null pos $ ct t)))]
            transformVarDecl (t, A.Init pos name@(A.Ident varpos x) e) = do
                (en, ecode) <- transform (ct t) e
                n <- newNameFor name (ct t)
                ent <- typeOf en
                return $ ecode ++ [B.VarDecl pos (ct t) n (B.Val pos (B.Var varpos en ent))]
    doTransform _ (A.Assignment _ el er) = do
        liftPipelineOpt $ printLogInfo $ T.pack $ "Assign " ++ (show el)
        case el of
            A.Var p (A.Ident _ x) -> do
                x' <- nameOf p x
                t <- typeOf x'
                (en, enc) <- transform t er
                ent <- typeOf en
                justEmit $ enc ++ [B.Assign p t (B.Variable p x') (B.Val p (B.Var p en ent))]
            A.ArrAccess p earr eidx _ -> do
                (en, enc) <- transform unknownType er
                (enarr, enarrc) <- transform (B.ArrT p $ unknownType) earr
                (enidx, enidxc) <- transform (B.IntT p) eidx
                t <- typeOf en
                enidxt <- typeOf enidx
                justEmit $ enc ++ enarrc ++ enidxc ++ [B.Assign p t (B.Array p enarr (B.Var p enidx enidxt)) (B.Val p (B.Var p en t))]
            A.Member p em (A.Ident _ field) (Just className) -> do
                (enm, enmc) <- transform (B.Reference p $ B.Label p className) em
                t <- getFieldType p className field
                (en, enc) <- transform t er
                ent <- typeOf en
                justEmit $ enc ++ enmc ++ [B.Assign p t (B.Member p enm (B.Label p className) (B.Label p field)) (B.Val p (B.Var p en ent))]
    doTransform _ (A.ReturnValue p e) = do
        retTypeContext <- lcStateGet (^. returnContextType)
        case retTypeContext of 
            (Just retType) -> do
                (en, enc) <- transform retType e
                t <- typeOf en
                justEmit $ enc ++ [B.ReturnVal p t (B.Val p (B.Var p en t))]
    doTransform _ (A.ReturnVoid p) = justEmit $ [B.Return p]
    doTransform _ (A.ExprStmt _ e) = do
        (_, enc) <- transform unknownType e
        justEmit $ enc
    doTransform _ (A.BlockStmt _ (A.Block _ stmts)) = justEmit =<< concat <$> mapM (transformOnly unknownType) stmts
    doTransform _ (A.IfElse p ec st sf) = do
        lif <- newLabel "_IIF"
        lelse <- newLabel "_IELSE"
        lend <- newLabel "_IEND"
        condc <- transformCondition ec lif lelse False
        stc <- transformOnly unknownType st
        sfc <- transformOnly unknownType sf
        justEmit $ condc ++ [B.SetLabel p lif] ++ stc ++ [B.Jump p lend, B.SetLabel p lelse] ++ sfc ++ [B.SetLabel p lend]
    doTransform _ (A.While p ec s) = do
        lcond <- newLabel "_WCOND"
        lbegin <- newLabel "_WBEG"
        lend <- newLabel "_WEND"
        sc <- transformOnly unknownType s
        ecc <- transformCondition ec lbegin lend True
        justEmit $ [B.Jump p lcond, B.SetLabel p lbegin] ++ sc ++ [B.SetLabel p lcond] ++ ecc