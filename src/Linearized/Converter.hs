module Linearized.Converter where

import qualified Program.Syntax as A
import qualified Linearized.Syntax as B

import qualified Utils.Containers.IDMap as IM
import qualified Data.Map as M
import Data.List

import Control.Lens hiding(transform, over)

import Reporting.Errors.Position
import qualified Reporting.Errors.Def as Errors
import Reporting.Logs
import Linearized.Syntax (IRPosition(..))
import Linearized.Env
import Linearized.Def

import Typings.Env as TypeChecker
import Typings.Types as Types

import Data.Maybe

import Control.Monad

class  (A.IsSyntax ma Position, B.IsIR mb) => IRAST ma mb where
    doTransform :: ma Position -> LinearConverter (mb Position)
    over :: [ma Position] -> LinearConverter [mb Position]
    over = mapM transform

    transform :: ma Position -> LinearConverter (mb Position)
    transform ast = doTransform ast
    -- return . B.modifyPos (\_ -> posFrom ast)  =<< 

data FnProto = FnProto Position (B.Label Position) (B.Type Position) [A.Arg Position] [A.Stmt Position]

newNameFor :: (A.Ident Position) -> (B.Type Position) -> LinearConverter (B.Name Position)
newNameFor (A.Ident pos n) t = do
    (B.Name _ n') <- newName t
    lcStateSet (\env -> env & varMap %~ M.insert n n')
    return $ B.Name pos n'

newName :: (B.Type Position) -> LinearConverter (B.Name Position)
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


ct :: (Show a) => A.Type a -> B.Type a
ct (A.BoolT p) = B.ByteT p
ct (A.ByteT p) = B.ByteT p
ct (A.VoidT p) = B.ByteT p
ct (A.IntT p) = B.IntT p
ct ast = B.Reference $ A.getPos ast

collectStructures :: [A.Definition Position] -> LinearConverter [(B.Structure Position)]
collectStructures defs = do
    structs <- IM.fromM (idMapFailure "collectStructures" $ Errors.ILNEDuplicateStructure) =<< return . concat =<< mapM collectFromDef defs
    lcStateSet (\env -> env & structures .~ structs)
    lcStateGet (\env -> IM.elems (env ^. structures))
    where
        collectFromDef :: A.Definition Position -> LinearConverter [(B.Structure Position)]
        collectFromDef def@(A.ClassDef _ (A.Ident _ name) _ _) = do
            tcEnv <- lcStateGet (^.typings)
            cls <- maybe (failure (\(tcEnv, lnEnv) -> Errors.InternalLinearizerFailure tcEnv lnEnv "collectFromDef" $ Errors.ILNEMissingClass name def)) return $ TypeChecker.findClassInEnv tcEnv name
            collectFromClass cls
        collectFromDef _ = return []
        collectFromClass :: Types.Class -> LinearConverter [(B.Structure Position)]
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
            chainMembers <- (return . concat) =<< (mapM ((return . map (\(B.Struct _ _ _ _ fields methods) -> (fields, methods))) <=< collectFromClass) parentChain)
            completeStruct <- foldM (\struct (fields, methods) -> classParentMerge clname struct fields methods) structPrototype chainMembers
            return [completeStruct]
        translateToMethod :: Types.Member -> Maybe (B.Label Position)
        translateToMethod (Types.Method (A.Ident p n) _ _ _) = Just (B.Label p n)
        translateToMethod _ = Nothing
        translateToField :: Types.Member -> Maybe (B.Label Position, B.Type Position, B.Offset)
        translateToField (Types.Field (A.Ident p n) t _) = Just (B.Label p n, ct t, 0)
        translateToField _ = Nothing
        classParentMerge :: String -> B.Structure Position -> (IM.Map (B.Label Position, B.Type Position, B.Offset)) -> (IM.Map (B.Label Position)) -> LinearConverter (B.Structure Position)
        classParentMerge clsName (B.Struct pos name parent offset fields methods) parentFields parentMethods = do
            combinedMethods <- IM.insertManyM (idMapFailure "classParentMerge" $ Errors.ILNEEncounteredDuplicateStructureMember name) (IM.mapList (\_ (B.Label lPos id) -> B.Label pos $ "_"++id++"_"++clsName) parentMethods) methods
            combinedFields <- IM.concatSequenceM (idMapFailure "classParentMerge" $ Errors.ILNEEncounteredDuplicateStructureMember name) (\m (l, t, o) -> (l, t, measureOffset m)) fields parentFields
            --(B.Label idpos $ "_class_"++id) name newParent
            --newParent <- return $ parent >>= (\(A.Label _ pid) -> return $ w"_class_"++pid)
            --IM.concatM (idMapFailure "classParentMerge" $ Errors.ILNEEncounteredDuplicateStructureField name) fields ()
            return $ B.Struct pos name parent (measureOffset combinedFields) combinedFields combinedMethods
        measureOffset :: (IM.Map (B.Label a, B.Type a, B.Offset)) -> B.Size
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
        collectFromClassDecl _ (A.FieldDecl _ _ _) = []
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
    nstmts <- over stmts
    nargs <- mapM (\(A.Arg _ t n) -> newNameFor n (ct t) <&> (,) (ct t)) args
    return $ B.Fun pos name retType nargs nstmts

instance IRAST A.Program B.Program where
    doTransform (A.Program a tds) = do
        fns <- collectFunctions tds
        structs <- collectStructures tds
        return $ B.Program a structs fns []

instance IRAST A.Stmt B.Stmt where
    doTransform ast = return $ B.Return $ A.getPos ast