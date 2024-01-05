module Linearized.Converter where

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

import qualified Typings.Env as TypeChecker
import qualified Typings.Types as Types

import Data.Maybe

import Control.Monad

class  (A.IsSyntax ma Position, B.IsIR mb) => IRAST ma mb where
    doTransform :: ma Position -> LinearConverter [mb Position]
    transformOver :: [ma Position] -> LinearConverter [mb Position]
    transformOver = return . concat <=< mapM transform

    transform :: ma Position -> LinearConverter [mb Position]
    transform = doTransform
    -- return . B.modifyPos (\_ -> posFrom ast)  =<< 

data FnProto = FnProto Position (B.Label Position) (B.Type Position) [A.Arg Position] [A.Stmt Position]

newNameFor :: A.Ident Position -> B.Type Position -> LinearConverter (B.Name Position)
newNameFor (A.Ident pos n) t = do
    (B.Name _ n') <- newName t
    lcStateSet (\env -> env & varMap %~ M.insert n n')
    return $ B.Name pos n'

newName :: B.Type Position -> LinearConverter (B.Name Position)
newName t = do
    i <- lcStateGet (^. varNameCounter)
    let n = "t_"++show i
    lcStateSet (\env -> env & varNameCounter %~ (+1) & varType %~ M.insert n t)
    return $ B.Name (B.getPos t) n

newLabel :: String -> LinearConverter (B.Label Position)
newLabel prefix = do
    i <- lcStateGet (^. varNameCounter)
    lcStateSet (\env -> env & varNameCounter %~ (+1))
    return (B.Label Undefined $ prefix++show i)

nameOf :: Position -> String -> LinearConverter (B.Name Position)
nameOf pos varName =
    maybe (failure (\(tcEnv, lnEnv) -> Errors.InternalLinearizerFailure tcEnv lnEnv "nameOf" $ Errors.ILNEMissingVariable varName)) (return . (B.Name pos)) . M.lookup varName =<< lcStateGet (^.varMap)

typeOf :: (B.Name Position) -> LinearConverter (B.Type Position)
typeOf (B.Name _ varName) =
    maybe (failure (\(tcEnv, lnEnv) -> Errors.InternalLinearizerFailure tcEnv lnEnv "typeOf" $ Errors.ILNEMissingVariable varName)) return . M.lookup varName =<< lcStateGet (^.varType)


ct :: (Show a) => A.Type a -> B.Type a
ct (A.BoolT p) = B.ByteT p
ct (A.ByteT p) = B.ByteT p
ct (A.VoidT p) = B.ByteT p
ct (A.IntT p) = B.IntT p
ct ast = B.Reference $ A.getPos ast

getMethod :: Position -> String -> String -> LinearConverter (B.Label Position, B.Index)
getMethod pos clsName methodName = do
    --cls <- maybe (failure (\(tcEnv, lnEnv) -> Errors.InternalLinearizerFailure tcEnv lnEnv "getMethodInfo" $ Errors.ILNEMissingClass name Nothing pos)) return $ TypeChecker.findClassInEnv tcEnv name
    struct@(B.Struct _ _ _ _ _ methods) <- join $ lcStateGet (\env -> IM.findM (idMapFailure "getMethod" (\clsName -> Errors.ILNEMissingClass ("_class_" ++ clsName) Nothing pos)) clsName (env ^. structures))
    (_, method, methodIndex) <- IM.findElemM (idMapFailure "getMethod" (`Errors.ILNEMissingMethod` struct)) methodName methods
    return (method, toInteger methodIndex)

getField :: Position -> String -> String -> LinearConverter (B.Label Position, B.Type Position, B.Offset)
getField pos clsName fieldName = do
    --cls <- maybe (failure (\(tcEnv, lnEnv) -> Errors.InternalLinearizerFailure tcEnv lnEnv "getMethodInfo" $ Errors.ILNEMissingClass name Nothing pos)) return $ TypeChecker.findClassInEnv tcEnv name
    struct@(B.Struct _ _ _ _ fields _) <- join $ lcStateGet (\env -> IM.findM (idMapFailure "getField" (\clsName -> Errors.ILNEMissingClass ("_class_" ++ clsName) Nothing pos)) clsName (env ^. structures))
    IM.findM (idMapFailure "getField" (`Errors.ILNEMissingMethod` struct)) fieldName fields

getFieldOffset :: Position -> String -> String -> LinearConverter B.Offset
getFieldOffset pos clsName = (\(_, _, o) -> return o) <=< getField pos clsName

getFieldType :: Position -> String -> String -> LinearConverter (B.Type Position)
getFieldType pos clsName = (\(_, t, _) -> return t) <=< getField pos clsName

getFunctionType :: B.Label Position -> LinearConverter (B.Type Position)
getFunctionType (B.Label pos fnName) =
    (\(B.Fun _ _ t _ _) -> return t) =<< join (lcStateGet (\env -> IM.findM (idMapFailure "getFunctionType" Errors.ILNEUndefinedFunction) fnName (env ^. functions)))

collectStructures :: [A.Definition Position] -> LinearConverter [B.Structure Position]
collectStructures defs = do
    structs <- IM.fromM (idMapFailure "collectStructures" Errors.ILNEDuplicateStructure) . concat =<< mapM collectFromDef defs
    lcStateSet (\env -> env & structures .~ structs)
    lcStateGet (\env -> IM.elems (env ^. structures))
    where
        collectFromDef :: A.Definition Position -> LinearConverter [B.Structure Position]
        collectFromDef def@(A.ClassDef pos (A.Ident _ name) _ _) = do
            tcEnv <- lcStateGet (^.typings)
            cls <- maybe (failure (\(tcEnv, lnEnv) -> Errors.InternalLinearizerFailure tcEnv lnEnv "collectFromDef" $ Errors.ILNEMissingClass name (Just def) pos)) return $ TypeChecker.findClassInEnv tcEnv name
            collectFromClass cls
        collectFromDef _ = return []
        collectFromClass :: Types.Class -> LinearConverter [B.Structure Position]
        collectFromClass def@(Types.Class (A.Ident clnamePos clname) parent members _) = do
            let newName = B.Label clnamePos $ "_class_" ++ clname
            tcEnv <- lcStateGet (^.typings)
            chain <- maybe (failure (\(tcEnv, lnEnv) -> Errors.InternalLinearizerFailure tcEnv lnEnv "collectStructures" $ Errors.ILNEMissingClassDefinition clname def)) return $ TypeChecker.findClassInheritanceChain tcEnv clname
            selfMethods <- IM.fromM (idMapFailure "collectFromClass" $ Errors.ILNEEncounteredDuplicateStructureMember newName) $ mapMaybe translateToMethod members
            selfFields <- IM.fromM (idMapFailure "collectFromClass" $ Errors.ILNEEncounteredDuplicateStructureMember newName) $ mapMaybe translateToField members
            let parentChain = reverse $ tail chain
            let newParent = case parent of
                    (A.NoName _) -> Nothing
                    (A.Name _ (A.Ident idpos id)) -> Just $ B.Label idpos $ "_class_"++id
            let structPrototype = B.Struct clnamePos newName newParent 0 selfFields selfMethods
            chainMembers <- concat <$> mapM (return . map (\(B.Struct _ _ _ _ fields methods) -> (fields, methods)) <=< collectFromClass) parentChain
            completeStruct <- foldM (\struct (fields, methods) -> classParentMerge clname struct fields methods) structPrototype chainMembers
            return [completeStruct]
        translateToMethod :: Types.Member -> Maybe (B.Label Position)
        translateToMethod (Types.Method (A.Ident p n) _ _ _) = Just (B.Label p n)
        translateToMethod _ = Nothing
        translateToField :: Types.Member -> Maybe (B.Label Position, B.Type Position, B.Offset)
        translateToField (Types.Field (A.Ident p n) t _) = Just (B.Label p n, ct t, 0)
        translateToField _ = Nothing
        classParentMerge :: String -> B.Structure Position -> IM.Map (B.Label Position, B.Type Position, B.Offset) -> IM.Map (B.Label Position) -> LinearConverter (B.Structure Position)
        classParentMerge clsName (B.Struct pos name parent offset fields methods) parentFields parentMethods = do
            combinedMethods <- IM.insertManyM (idMapFailure "classParentMerge" $ Errors.ILNEEncounteredDuplicateStructureMember name) (IM.mapList (\_ (B.Label lPos id) -> B.Label pos $ "_"++id++"_"++clsName) parentMethods) methods
            combinedFields <- IM.concatSequenceM (idMapFailure "classParentMerge" $ Errors.ILNEEncounteredDuplicateStructureMember name) (\m (l, t, o) -> (l, t, measureOffset m)) fields parentFields
            --(B.Label idpos $ "_class_"++id) name newParent
            --newParent <- return $ parent >>= (\(A.Label _ pid) -> return $ w"_class_"++pid)
            --IM.concatM (idMapFailure "classParentMerge" $ Errors.ILNEEncounteredDuplicateStructureField name) fields ()
            return $ B.Struct pos name parent (measureOffset combinedFields) combinedFields combinedMethods
        measureOffset :: IM.Map (B.Label a, B.Type a, B.Offset) -> B.Size
        measureOffset m = case IM.last m of
            Nothing -> 0
            (Just (_, B.IntT _, o)) -> o + 0x04
            (Just (_, B.ByteT _, o)) -> o + 0x04
            (Just (_, B.Reference _, o)) -> o + 0x08

-- collectFn (A.FunctionDef _ t (A.Ident _ name) args block) = do
--             if elem name (map (\(B.Fun l _ _ _) ->l) funcs) then
--                 return []--transF name args block
--             else return [toEntity $ B.Fun name (ct t) [] []]
collectFunctions :: [A.Definition Position] -> LinearConverter [B.Function Position]
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
transformFunction :: FnProto -> LinearConverter (B.Function Position)
transformFunction (FnProto pos name@(B.Label _ id) retType args stmts) = do
    nstmts <- transformOver stmts
    nargs <- mapM (\(A.Arg _ t n) -> newNameFor n (ct t) <&> (,) (ct t)) args
    return $ B.Fun pos name retType nargs nstmts

transformProgram :: (A.Program Position) -> LinearConverter (B.Program Position)
transformProgram prog = do
    nprogs <- transform prog
    let (poss, structs, fns, strs) = unzip4 $ map (\(B.Program pos structs fns strs) -> (pos, structs, fns, strs)) nprogs
    structsMap <- IM.concatMapsM (idMapFailure "transformProgram" Errors.ILNEDuplicateStructure) structs
    fnsMap <- IM.concatMapsM (idMapFailure "transformProgram" Errors.ILNEDuplicateFunctionName) fns
    strsMap <- IM.concatMapsM (idMapFailure "transformProgram" Errors.ILNEDuplicateLabelledString) strs
    return $ B.Program (head poss) structsMap fnsMap strsMap

instance IRAST A.Program B.Program where
    doTransform (A.Program a tds) = do
        fns <- IM.fromM (idMapFailure "transformProgram" Errors.ILNEDuplicateFunctionName) =<< collectFunctions tds
        structs <- IM.fromM (idMapFailure "transformProgram" Errors.ILNEDuplicateStructure) =<<  collectStructures tds
        strs <- return $ IM.empty
        return [B.Program a structs fns strs]

transformExpr :: (A.Expr Position) -> LinearConverter (B.Name Position, [B.Stmt Position])
transformExpr ast = do
    n <- newName $ B.Reference $ A.getPos ast
    return (n, [])

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


transformCondition :: (A.Expr Position) -> (B.Label Position) -> (B.Label Position) -> Bool -> LinearConverter [B.Stmt Position]
transformCondition (A.UnaryOp p (A.Not _) e) ltrue lfalse neg =
    transformCondition e lfalse ltrue (not neg)
transformCondition (A.BinaryOp p op el er) ltrue lfalse neg =
    if A.isAA op then do
        (nl, nlc) <- transformExpr el
        (nr, nrc) <- transformExpr er
        case neg of
            False -> return $ nlc ++ nrc ++ [B.JumpCmp p (opNeg (opC op)) lfalse (B.Var p nl) (B.Var p nr)]
            True -> return $ nlc ++ nrc ++ [B.JumpCmp p (opC op) ltrue (B.Var p nl) (B.Var p nr)]
    else case op of
        (A.And _) -> do
            elc <- transformCondition el ltrue lfalse False
            erc <- transformCondition er ltrue lfalse neg
            return $ elc ++ erc
        (A.Or p) -> do
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
            (n, ec) <- transformExpr e
            case neg of
                False -> return $ ec ++ [B.JumpCmp p (B.Eq p) lfalse (B.Var p n) (B.Const p $ B.ByteC p 0)]
                True -> return $ ec ++ [B.JumpCmp p (B.Ne p) ltrue (B.Var p n) (B.Const p $ B.ByteC p 0)]

instance IRAST A.Stmt B.Stmt where
    --doTransform ast = return [B.Return $ A.getPos ast]

    doTransform (A.Empty _) = return []
    doTransform (A.VarDecl _ decls) = concat <$> mapM transformVarDecl decls
        where
            transformVarDecl :: (A.Type Position, A.DeclItem Position) -> LinearConverter [B.Stmt Position]
            transformVarDecl (t, A.NoInit pos name) = do
                n <- newNameFor name (ct t)
                return [B.VarDecl pos (ct t) n (B.Val pos (B.Const pos (B.Null pos)))]
            transformVarDecl (t, A.Init pos name@(A.Ident varpos x) e) = do
                (en, ecode) <- transformExpr e
                n <- newNameFor name (ct t)
                return $ ecode ++ [B.VarDecl pos (ct t) n (B.Val pos (B.Var varpos en))]
    doTransform (A.Assignment _ el er) = do
        (en, enc) <- transformExpr er
        case el of
            A.Var p (A.Ident _ x) -> do
                x' <- nameOf p x
                t <- typeOf x'
                return $ enc ++ [B.Assign p t (B.Variable p x') (B.Val p (B.Var p en))]
            A.ArrAccess p earr eidx _ -> do
                (enarr, enarrc) <- transformExpr earr
                (enidx, enidxc) <- transformExpr eidx
                t <- typeOf en
                return $ enc ++ enarrc ++ enidxc ++ [B.Assign p t (B.Array p enarr (B.Var p enidx)) (B.Val p (B.Var p en))]
            A.Member p em (A.Ident _ field) (Just className) -> do
                (enm, enmc) <- transformExpr em
                off <- getFieldOffset p className field
                t <- getFieldType p className field
                return $ enc ++ enmc ++ [B.Assign p t (B.Member p enm off) (B.Val p (B.Var p en))]
    doTransform (A.ReturnValue p e) = do
        (en, enc) <- transformExpr e
        t <- typeOf en
        return $ enc ++ [B.ReturnVal p t (B.Val p (B.Var p en))]
    doTransform (A.ReturnVoid p) = return [B.Return p]
    doTransform (A.ExprStmt _ e) = do
        (_, enc) <- transformExpr e
        return enc
    doTransform (A.BlockStmt _ (A.Block _ stmts)) = concat <$> mapM transform stmts
    doTransform (A.IfElse p ec st sf) = do
        lif <- newLabel "_IIF"
        lelse <- newLabel "_IELSE"
        lend <- newLabel "_IEND"
        condc <- transformCondition ec lif lelse False
        stc <- transform st
        sfc <- transform sf
        return $ condc ++ [B.SetLabel p lif] ++ stc ++ [B.Jump p lend, B.SetLabel p lelse] ++ sfc ++ [B.SetLabel p lend]
    doTransform (A.While p ec s) = do
        lcond <- newLabel "_WCOND"
        lbegin <- newLabel "_WBEG"
        lend <- newLabel "_WEND"
        sc <- transform s
        ecc <- transformCondition ec lbegin lend True
        return $ [B.Jump p lcond, B.SetLabel p lbegin] ++ sc ++ [B.SetLabel p lcond] ++ ecc