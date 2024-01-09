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

import qualified Data.Text as T

import qualified Typings.Env as TypeChecker
import qualified Typings.Types as Types

import Data.Maybe
import Data.Bifunctor

import Control.Monad

class  (A.IsSyntax ma Position, B.IsIR mb) => IRConvertable ma mb c | ma -> mb, ma -> c where
    doTransform :: ma Position -> LinearConverter () (c, [mb Position])
    transformOver :: [ma Position] -> LinearConverter () (c, [mb Position])
    transformOver = return . (\(c, ast) -> (head c, concat ast)) . unzip <=< mapM transform

    transformOverOnly :: [ma Position] -> LinearConverter () [mb Position]
    transformOverOnly = return . snd <=< transformOver

    transformOnly :: ma Position -> LinearConverter () [mb Position]
    transformOnly = return . snd <=< transform

    transform :: ma Position -> LinearConverter () (c, [mb Position])
    transform ast = do
        --liftPipelineOpt $ printLogInfo $ T.pack $ "Transform to IR: " ++ (show ast)
        r <- doTransform ast
        --liftPipelineOpt $ printLogInfo $ T.pack $ "[DONE] Transform to IR: " ++ (show ast)
        return r
    -- return . B.modifyPos (\_ -> posFrom ast)  =<< 

justEmit :: [mb Position] -> LinearConverter () ((), [mb Position])
justEmit = return . (,) ()

noEmit :: LinearConverter () ((), [mb Position])
noEmit = justEmit []

stripClassName :: String -> String
stripClassName pm = case findIndex (== '_') (drop 1 pm) of
    Just i -> drop (i+2) pm

data FnProto = FnProto Position (B.Label Position) (B.Type Position) [A.Arg Position] [A.Stmt Position]

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
ct ast = B.Reference $ A.getPos ast

getMethod :: Position -> String -> String -> LinearConverter () (B.Label Position, B.Index)
getMethod pos clsName methodName = do
    --cls <- maybe (failure (\(tcEnv, lnEnv) -> Errors.InternalLinearizerFailure tcEnv lnEnv "getMethodInfo" $ Errors.ILNEMissingClass name Nothing pos)) return $ TypeChecker.findClassInEnv tcEnv name
    struct@(B.Struct _ _ _ _ _ methods) <- join $ lcStateGet (\env -> IM.findM (idMapFailure "getMethod" (\clsName -> Errors.ILNEMissingClass (clsName) Nothing pos)) ("_class_"++clsName) (env ^. structures))
    existingMethodName <- maybe (failure (\(tcEnv, lnEnv) -> Errors.InternalLinearizerFailure tcEnv lnEnv "getMethod" $ Errors.ILNEMissingMethod methodName struct)) (return) $ listToMaybe $ filter (\name -> stripClassName name == methodName) $ IM.mapList (\name _ -> name) methods
    (_, method, methodIndex) <- IM.findElemM (idMapFailure "getMethod" (`Errors.ILNEMissingMethod` struct)) (existingMethodName) methods
    return (method, toInteger methodIndex)

getField :: Position -> String -> String -> LinearConverter () (B.Label Position, B.Type Position, B.Offset)
getField pos clsName fieldName = do
    --liftPipelineOpt $ printLogInfoStr $ "get field all structurs are " ++ (show pppp)
    --cls <- maybe (failure (\(tcEnv, lnEnv) -> Errors.InternalLinearizerFailure tcEnv lnEnv "getMethodInfo" $ Errors.ILNEMissingClass name Nothing pos)) return $ TypeChecker.findClassInEnv tcEnv name
    struct@(B.Struct _ _ _ _ fields _) <- join $ lcStateGet (\env -> IM.findM (idMapFailure "getField" (\clsName -> Errors.ILNEMissingClass (clsName) Nothing pos)) ("_class_"++clsName) (env ^. structures))
    IM.findM (idMapFailure "getField" (`Errors.ILNEMissingMethod` struct)) fieldName fields

getFieldOffset :: Position -> String -> String -> LinearConverter () B.Offset
getFieldOffset pos clsName = (\(_, _, o) -> return o) <=< getField pos clsName

getFieldType :: Position -> String -> String -> LinearConverter () (B.Type Position)
getFieldType pos clsName = (\(_, t, _) -> return t) <=< getField pos clsName

getFunctionType :: B.Label Position -> LinearConverter () (B.Type Position)
getFunctionType (B.Label pos fnName) =
    (\(B.Fun _ _ t _ _) -> return t) =<< join (lcStateGet (\env -> IM.findM (idMapFailure "getFunctionType" Errors.ILNEUndefinedFunction) fnName (env ^. functions)))

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
            let newName = B.Label clnamePos $ "_class_" ++ clname
            tcEnv <- lcStateGet (^.typings)
            chain <- maybe (failure (\(tcEnv, lnEnv) -> Errors.InternalLinearizerFailure tcEnv lnEnv "collectStructures" $ Errors.ILNEMissingClassDefinition clname def)) return $ TypeChecker.findClassInheritanceChain tcEnv clname
            selfMethods <- IM.fromM (idMapFailure "collectFromClass" $ Errors.ILNEEncounteredDuplicateStructureMember newName) $ mapMaybe (translateToMethod clname) members
            selfFields <- IM.fromM (idMapFailure "collectFromClass" $ Errors.ILNEEncounteredDuplicateStructureMember newName) $ mapMaybe (translateToField clname) members
            let parentChain = reverse $ tail chain
            let newParent = case parent of
                    (A.NoName _) -> Nothing
                    (A.Name _ (A.Ident idpos id)) -> Just $ B.Label idpos $ "_class_"++id
            let structPrototype = B.Struct clnamePos newName newParent 0 IM.empty IM.empty
            chainMembers <- concat <$> mapM (return . map (\(B.Struct _ (B.Label _ name) _ _ fields methods) -> (stripClassName name, fields, methods)) <=< collectFromClass) parentChain
            completeStruct <- foldM (\struct (name, fields, methods) -> classParentMerge clname name struct fields methods) structPrototype chainMembers
            completeStruct' <- classParentMerge clname clname completeStruct selfFields selfMethods
            return [completeStruct']
        translateToMethod :: String -> Types.Member -> Maybe (B.Label Position)
        translateToMethod clname (Types.Method (A.Ident p n) _ _ _) = Just (B.Label p $ "_" ++ clname ++ "_" ++ n)
        translateToMethod _ _ = Nothing
        translateToField :: String -> Types.Member -> Maybe (B.Label Position, B.Type Position, B.Offset)
        translateToField _ (Types.Field (A.Ident p n) t _) = Just (B.Label p n, ct t, 0)
        translateToField _ _ = Nothing
        classParentMerge :: String -> String -> B.Structure Position -> IM.Map (B.Label Position, B.Type Position, B.Offset) -> IM.Map (B.Label Position) -> LinearConverter () (B.Structure Position)
        classParentMerge clsName parentName (B.Struct pos name parent offset fields methods) parentFields parentMethods = do
            -- (IM.mapList (\_ (B.Label lPos id) -> B.Label pos $ "_"++clsName++"_z"++stripClassName id) parentMethods)
            let overridenMethods = filter (\name -> let pkeys = IM.keys parentMethods in any (\pname -> stripClassName name == stripClassName pname) pkeys) $ IM.keys methods
            let newMethods = (IM.mapList (\_ (B.Label lPos id) -> B.Label pos $ "_"++parentName++"_"++stripClassName id) parentMethods)
            combinedMethods <- IM.insertManyM (idMapFailure "classParentMerge" $ Errors.ILNEEncounteredDuplicateStructureMember name) newMethods $ IM.deleteMany overridenMethods methods
            --combinedMethods <- IM.insertManyM (idMapFailure "classParentMerge" $ Errors.ILNEEncounteredDuplicateStructureMember name) (IM.mapList (\_ (B.Label lPos id) -> B.Label pos $ "_"++clsName++"_z"++stripClassName id) parentMethods) methods
            combinedFields <- IM.concatSequenceM (idMapFailure "classParentMerge" $ Errors.ILNEEncounteredDuplicateStructureMember name) (\m (l, t, o) -> (l, t, measureOffset m)) parentFields fields
            --(B.Label idpos $ "_class_"++id) name newParent
            --newParent <- return $ parent >>= (\(A.Label _ pid) -> return $ w"_class_"++pid)
            --IM.concatM (idMapFailure "classParentMerge" $ Errors.ILNEEncounteredDuplicateStructureField name) fields ()
            return $ B.Struct pos name parent (measureOffset combinedFields) combinedFields combinedMethods
        measureOffset :: IM.Map (B.Label a, B.Type a, B.Offset) -> B.Size
        measureOffset m = case IM.last m of
            Nothing -> 0
            (Just (_, B.IntT _, o)) -> o + 0x04
            (Just (_, B.ByteT _, o)) -> o + 0x01
            (Just (_, B.Reference _, o)) -> o + 0x08

-- collectFn (A.FunctionDef _ t (A.Ident _ name) args block) = do
--             if elem name (map (\(B.Fun l _ _ _) ->l) funcs) then
--                 return []--transF name args block
--             else return [toEntity $ B.Fun name (ct t) [] []]
collectFunctions :: [A.Definition Position] -> LinearConverter () [B.Function Position]
collectFunctions defs = do
    let fns = concatMap collectFromDef defs
    fnPrototypes <- IM.fromM (idMapFailure "collectFunctions" Errors.ILNEDuplicateFunctionName) $ builtIns ++ map (\(FnProto pos name@(B.Label _ id) retType _ _) -> B.Fun pos name retType [] []) fns
    lcStateSet (\env -> env & functions .~ fnPrototypes)
    --liftPipelineOpt $ printLogInfo $ T.pack $ show fnPrototypes
    mapM_ (transformFunction >=> overrideFunction) fns
    lcStateGet (\env -> IM.elems (env ^. functions) \\ builtIns)
    where
        collectFromDef :: A.Definition Position -> [FnProto]
        collectFromDef (A.FunctionDef pos t (A.Ident idpos name) args (A.Block _ stmts)) = [FnProto pos (B.Label idpos name) (ct t) args stmts]
        collectFromDef (A.ClassDef _ (A.Ident _ clname) _ mems) = concatMap (collectFromClassDecl clname) mems
        collectFromClassDecl :: String -> A.ClassDecl Position -> [FnProto]
        collectFromClassDecl _ (A.FieldDecl {}) = []
        collectFromClassDecl clname (A.MethodDecl pos t (A.Ident idpos n) args  (A.Block _ stmts)) =
            [FnProto pos (B.Label idpos ("_"++clname++"_"++n)) (ct t) (A.Arg Undefined (A.ClassT Undefined (A.Ident Undefined clname)) (A.Ident Undefined "this") : args) stmts]
        builtIns =
            [
                B.Fun BuiltIn (B.Label BuiltIn "_Array_toString") (B.Reference BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "_Object_toString") (B.Reference BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "_Object_getHashCode") (B.IntT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "_Object_equals") (B.ByteT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "_String_equals") (B.ByteT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "_String_getHashCode") (B.IntT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "_String_toString") (B.Reference BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "_String_substring") (B.Reference BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "_String_length") (B.IntT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "_String_indexOf") (B.IntT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "_String_getBytes") (B.Reference BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "_String_endsWith") (B.ByteT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "_String_startsWith") (B.ByteT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "_String_concat") (B.Reference BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "_String_charAt") (B.IntT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "printString") (B.ByteT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "printInt") (B.ByteT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "printByte") (B.ByteT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "printBoolean") (B.ByteT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "printBinArray") (B.ByteT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "byteToString") (B.Reference BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "boolToString") (B.Reference BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "intToString") (B.Reference BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "print") (B.ByteT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "error") (B.ByteT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "readInt") (B.IntT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "readString") (B.Reference BuiltIn) [] []
            ]
transformFunction :: FnProto -> LinearConverter () (B.Function Position)
transformFunction (FnProto pos name@(B.Label _ id) retType args stmts) = do
    nargs <- mapM (\(A.Arg _ t n) -> newNameFor n (ct t) <&> (,) (ct t)) args
    nstmts <- transformOverOnly stmts
    return $ B.Fun pos name retType nargs nstmts

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
    doTransform (A.Lit pos (A.String _ s)) = do
        (B.DataString _ _ strLabel, newDatas) <- join $ lcStateGet (\env -> IM.provideM (\_ -> (\l -> B.DataString pos s l) <$> newLabel "_S") s $ env ^. datas)
        lcStateSet (\env -> env & datas .~ newDatas)
        n <- newName $ B.Reference pos
        return (n, [B.VarDecl pos (B.Reference pos) n (B.NewString pos strLabel)])
    doTransform (A.Lit p l) = do
        n <- newName (litType l)
        let c = litC l
        return (n, [B.VarDecl p (litType l) n (B.Val p (B.Const p c))])
        where
            litType :: (A.Lit Position) -> (B.Type Position)
            litType l = case l of 
                            A.Null p -> B.Reference p
                            A.Int p _ -> B.IntT p
                            A.Byte p _ -> B.ByteT p
                            A.Bool p _ -> B.ByteT p
            litC :: (A.Lit Position) -> (B.Constant Position)
            litC l = case l of                    
                        A.Null p -> B.Null p
                        A.Int p i -> B.IntC p i
                        A.Byte p i -> B.ByteC p i
                        A.Bool p True -> B.ByteC p 1
                        A.Bool p False -> B.ByteC p 0
    doTransform (A.Var p (A.Ident _ x)) = ((flip (,)) []) <$> nameOf p x
    doTransform (A.Member p e (A.Ident _ field) (Just className)) = do
        (enm, enmc) <- transform e
        off <- getFieldOffset p className field
        t <- getFieldType p className field
        n <- newName t
        return (n, enmc ++ [B.VarDecl p t n (B.MemberAccess p enm off)])
    doTransform (A.NewObj p t m) = 
        case m of
            Nothing -> do
                let (A.ClassT _ (A.Ident _ cls)) = t
                n <- newName (ct t)
                return (n, [B.VarDecl p (ct t) n (B.NewObj p $ B.Label p $ "_class_"++cls)])
            Just e -> do
                (en, enc) <- transform e
                let tt = ct t
                n <- newName $ B.Reference p
                return (n, enc ++ [B.VarDecl p (B.Reference p) n (B.NewArray p tt (B.Var p en))])
    doTransform (A.ArrAccess p el er (Just t)) = do
        (enl, enlc) <- transform el
        (enr, enrc) <- transform er
        n <- newName (ct t)
        return (n, enlc ++ enrc ++ [B.VarDecl p (ct t) n (B.ArrAccess p enl (B.Var p enr))])
    doTransform (A.Cast p t e) = do
        (en, enc) <- transform e
        ent <- typeOf en
        case (ct t, ent) of
            (B.ByteT _, B.ByteT _) -> return (en, enc)
            (B.IntT _, B.IntT _) -> return (en, enc)
            (B.Reference _, B.Reference _) -> 
                case t of
                    A.ClassT p (A.Ident _ clsName) -> do
                        n <- newName (B.Reference p)
                        return (n, enc ++ [B.VarDecl p (B.Reference p) n (B.Cast p (B.Label p $ "_class_"++clsName) (B.Var p en))])
            (B.ByteT p', B.IntT _) -> do
                n <- newName $ B.ByteT p'
                return (n, enc ++ [B.VarDecl p (B.ByteT p) n (B.IntToByte p (B.Var p en))])
            (B.IntT p', B.ByteT _) -> do
                n <- newName $ B.IntT p'
                return (n, enc ++ [B.VarDecl p (B.IntT p) n (B.ByteToInt p (B.Var p en))])
    doTransform (A.UnaryOp _ op e) = do
        (en, enc) <- transform e
        ent <- typeOf en
        case op of
            A.Neg p -> do
                n <- newName ent
                let zero = case ent of {(B.IntT p) -> (B.IntC p 0); (B.ByteT p) -> (B.ByteC p 0)}
                return (n, enc ++ [B.VarDecl p ent n (B.BinOp p (B.Sub p) (B.Const p zero) (B.Var p en))])
            A.Not p -> do
                n <- newName ent
                return (n, enc ++ [B.VarDecl p ent n (B.Not p (B.Var p en))])
    doTransform (A.BinaryOp p (A.Add p2) (A.UnaryOp _ (A.Neg _) el) er) = transform (A.BinaryOp p (A.Sub p2) er el)
    doTransform (A.BinaryOp p (A.Add p2) el (A.UnaryOp _ (A.Neg _) er)) = transform (A.BinaryOp p (A.Sub p2) el er)
    doTransform e@(A.BinaryOp _ op el er) = 
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
                (enl, enlc) <- transform el
                (enr, enrc) <- transform er
                ent <- typeOf enl
                n <- newName ent
                let (bop, isReversed) = case op of
                            A.Add p -> (B.Add p, False)
                            A.Sub p -> (B.Sub p, True)
                            A.Mul p -> (B.Mul p, False)
                            A.Div p -> (B.Div p, True)
                            A.Mod p -> (B.Mod p, True)
                if isLit el && not isReversed then
                    return (n, enlc ++ enrc ++ [B.VarDecl p ent n (B.BinOp p bop (B.Var p enr) (B.Var p enl))])
                else
                    return (n, enlc ++ enrc ++ [B.VarDecl p ent n (B.BinOp p bop (B.Var p enl) (B.Var p enr))])
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
    doTransform (A.App _ el es) = do
        (ens, ensc) <- second concat <$> unzip <$> mapM transform es
        case el of
            (A.Var p (A.Ident p' f)) -> do
                let f' = B.Label p' f
                t <- getFunctionType f'
                n <- newName t
                return (n, ensc ++ [B.VarDecl p t n (B.Call p f' (map (B.Var p) ens))])
            (A.Member p e (A.Ident _ m) (Just clsName)) -> do
                (en, enc) <- transform e
                (l,i) <- getMethod p clsName m
                t <- getFunctionType l
                n <- newName t
                return (n, ensc ++ enc ++ [B.VarDecl p t n (B.MCall p en i (map (B.Var p) (en:ens)))])

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
        (nl, nlc) <- transform el
        (nr, nrc) <- transform er
        case neg of
            False -> return $ nlc ++ nrc ++ [B.JumpCmp p (opNeg (opC op)) lfalse (B.Var p nl) (B.Var p nr)]
            True -> return $ nlc ++ nrc ++ [B.JumpCmp p (opC op) ltrue (B.Var p nl) (B.Var p nr)]
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
            (n, ec) <- transform e
            case neg of
                False -> return $ ec ++ [B.JumpCmp p (B.Eq p) lfalse (B.Var p n) (B.Const p $ B.ByteC p 0)]
                True -> return $ ec ++ [B.JumpCmp p (B.Ne p) ltrue (B.Var p n) (B.Const p $ B.ByteC p 0)]

instance IRConvertable A.Stmt B.Stmt () where
    --doTransform ast = return [B.Return $ A.getPos ast]

    doTransform (A.Empty _) = noEmit
    doTransform (A.VarDecl _ decls) = justEmit =<< concat <$> mapM transformVarDecl decls
        where
            transformVarDecl :: (A.Type Position, A.DeclItem Position) -> LinearConverter () [B.Stmt Position]
            transformVarDecl (t, A.NoInit pos name) = do
                n <- newNameFor name (ct t)
                return $ [B.VarDecl pos (ct t) n (B.Val pos (B.Const pos (B.Null pos)))]
            transformVarDecl (t, A.Init pos name@(A.Ident varpos x) e) = do
                (en, ecode) <- transform e
                n <- newNameFor name (ct t)
                return $ ecode ++ [B.VarDecl pos (ct t) n (B.Val pos (B.Var varpos en))]
    doTransform (A.Assignment _ el er) = do
        (en, enc) <- transform er
        liftPipelineOpt $ printLogInfo $ T.pack $ "Assign " ++ (show el)
        case el of
            A.Var p (A.Ident _ x) -> do
                x' <- nameOf p x
                t <- typeOf x'
                justEmit $ enc ++ [B.Assign p t (B.Variable p x') (B.Val p (B.Var p en))]
            A.ArrAccess p earr eidx _ -> do
                (enarr, enarrc) <- transform earr
                (enidx, enidxc) <- transform eidx
                t <- typeOf en
                justEmit $ enc ++ enarrc ++ enidxc ++ [B.Assign p t (B.Array p enarr (B.Var p enidx)) (B.Val p (B.Var p en))]
            A.Member p em (A.Ident _ field) (Just className) -> do
                (enm, enmc) <- transform em
                off <- getFieldOffset p className field
                t <- getFieldType p className field
                justEmit $ enc ++ enmc ++ [B.Assign p t (B.Member p enm off) (B.Val p (B.Var p en))]
    doTransform (A.ReturnValue p e) = do
        (en, enc) <- transform e
        t <- typeOf en
        justEmit $ enc ++ [B.ReturnVal p t (B.Val p (B.Var p en))]
    doTransform (A.ReturnVoid p) = justEmit $ [B.Return p]
    doTransform (A.ExprStmt _ e) = do
        (_, enc) <- transform e
        justEmit $ enc
    doTransform (A.BlockStmt _ (A.Block _ stmts)) = justEmit =<< concat <$> mapM transformOnly stmts
    doTransform (A.IfElse p ec st sf) = do
        lif <- newLabel "_IIF"
        lelse <- newLabel "_IELSE"
        lend <- newLabel "_IEND"
        condc <- transformCondition ec lif lelse False
        stc <- transformOnly st
        sfc <- transformOnly sf
        justEmit $ condc ++ [B.SetLabel p lif] ++ stc ++ [B.Jump p lend, B.SetLabel p lelse] ++ sfc ++ [B.SetLabel p lend]
    doTransform (A.While p ec s) = do
        lcond <- newLabel "_WCOND"
        lbegin <- newLabel "_WBEG"
        lend <- newLabel "_WEND"
        sc <- transformOnly s
        ecc <- transformCondition ec lbegin lend True
        justEmit $ [B.Jump p lcond, B.SetLabel p lbegin] ++ sc ++ [B.SetLabel p lcond] ++ ecc