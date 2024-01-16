{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Linearized.IRConverter(initialState, run) where

import Control.Lens
import Control.DeepSeq
import Control.Monad
import GHC.Generics (Generic)

import qualified Utils.Containers.IDMap as IM

import Linearized.BuiltIns(builtIns)

import qualified IR.Syntax.Syntax as B
import qualified Linearized.Syntax as A

import Reporting.Logs
import qualified Reporting.Errors.Def as Errors

import Linearized.Def

data IRConverterEnv a = IRConverterEnv
  {
    _ircvTempVarNameCounter :: Int
    , _ircvProgDatas :: IM.Map (A.DataDef a)
  } deriving (Show, Generic, NFData)

makeLensesWith abbreviatedFields ''IRConverterEnv

initialState :: IRConverterEnv a
initialState = IRConverterEnv {
    _ircvTempVarNameCounter = 0
    , _ircvProgDatas = IM.empty
}

extractMethodDef :: (B.Method a) -> (B.MethodDef a)
extractMethodDef (B.Mthd p rt l params _) = 
    B.MthdDef p (B.FType p rt $ map (\(B.Param _ t _) -> t) params) l

run :: (A.Program a) -> LinearConverter (IRConverterEnv a) (B.Program a)
run prog@(A.Program p structs fns datas) = do
    oStateSet (\env -> env & progDatas .~ datas)
    liftPipelineOpt $ printLogInfoStr $ "AST to convert:\n" ++ (show prog)
    classDefs <- IM.mapListM (\_ struct -> convertStructureToFIR struct) structs
    methodsFns <- IM.mapListM (\_ fn -> convertFunctionToFIR fn) fns
    builtinFns <- return $ map (\(A.Fun _ cls l rt args stmts) -> B.MthdDef p (B.FType p (convertType rt) $ map (convertType . fst) args) (functionName cls l)) $ map (fmap (const p)) builtIns
    globalClassDef <- return $ B.ClDef p (B.SymIdent "~cl_TopLevel") [] [] (builtinFns ++ map extractMethodDef methodsFns)
    return $ B.Program p (B.Meta p $ classDefs ++ [globalClassDef]) methodsFns

convertStructureToFIR :: (A.Structure a) -> LinearConverter (IRConverterEnv a) (B.ClassDef a)
convertStructureToFIR (A.Struct p (A.Label _ name) chain methods fields) = do
    --(A.Label fieldPos fieldName), fieldType, _
    fields <- return $ IM.mapList (\_ (A.Field fieldPos fieldType (A.Label _ fieldName)) -> B.FldDef fieldPos (convertType fieldType) $ B.SymIdent fieldName) fields
    methods <- return $ IM.mapList (\_ (A.Method methodPos methodCls methodName methodType methodArgs) -> B.MthdDef methodPos (B.FType methodPos (convertType methodType) (map (convertType . fst) methodArgs)) (functionName (Just methodCls) methodName)) methods
    return $ B.ClDef p (B.SymIdent name) (map (\(A.Label _ pCls) -> B.SymIdent pCls) chain) fields methods

convertFunctionToFIR :: (A.Function a) -> LinearConverter (IRConverterEnv a) (B.Method a)
convertFunctionToFIR (A.Fun p cls l rt args stmts) = do
    instrs <- return . concat =<< mapM convertStmtToFIR stmts
    params <- return $ map (\(t, n) -> B.Param p (convertType t) (argNameToValIdent n)) args
    -- forM_ (zip argDecls [0..]) (\((_, val), i) -> emit $ ILoad () (valName val) (PParam () (Ref () $ valType_ val) i (valName val)))
    --B.ISet p (nameToValIdent n) (B.VVal p (convertType t) $ (argNameToValIdent n))
    paramsMoves <- return $ map (\((t, n), i) -> B.ILoad p (nameToValIdent n) (B.PParam p ((B.Ref p) $ convertType t) i (argNameToValIdent n))) $ zip args [0..]
    return $ B.Mthd p (convertType rt) (functionName cls l) params $ [B.ILabel p (B.LabIdent ".L_entry")] ++ paramsMoves ++ instrs ++ [B.ILabel p $ B.LabIdent ".L_exit", B.IRet p $ B.VVal p (convertType rt) $ B.ValIdent $ "%v_return"]
--Mthd a (SType a) (QIdent a) [Param a] [Instr a]
--Fun a (Label a) (Type a) [{-args-}(Type a, Name a)] [Stmt a]

newRetTempName :: a -> LinearConverter (IRConverterEnv a) (A.Name a)
newRetTempName p = do
    tvnc <- oStateGet (^. tempVarNameCounter)
    oStateSet (\env -> env & tempVarNameCounter %~ (+1))
    return $ A.Name p $ "__temp_" ++ (show tvnc)

convertType :: A.Type a -> B.SType a
convertType (A.IntT p) = B.Int p
convertType (A.ByteT p) = B.Int p
convertType (A.Reference p (A.Label _ clname)) = B.Ref p $ B.Cl p $ B.SymIdent clname
convertType (A.ArrT p t) = B.Ref p $ B.Arr p (convertType t)

functionName :: (Maybe (A.Label a)) -> A.Label a -> B.QIdent a
functionName Nothing (A.Label p name) = B.QIdent p (B.SymIdent "~cl_TopLevel") (B.SymIdent name)
functionName (Just (A.Label _ cls)) (A.Label p name) = B.QIdent p (B.SymIdent cls) (B.SymIdent name)

labelToValIdent :: A.Label a -> B.ValIdent
labelToValIdent (A.Label _ name) = B.ValIdent name

nameToValIdent :: A.Name a -> B.ValIdent
nameToValIdent (A.Name _ name) = B.ValIdent $ "%v_" ++ name

argNameToValIdent :: A.Name a -> B.ValIdent
argNameToValIdent (A.Name _ name) = B.ValIdent $ "%a_" ++ name


classType :: A.Label a -> B.SType a
classType (A.Label p name) = B.Cl p $ B.SymIdent name

convertOp :: A.Op a -> B.Op a 
convertOp (A.Add p) = B.OpAdd p
convertOp (A.Sub p) = B.OpSub p
convertOp (A.Mul p) = B.OpMul p
convertOp (A.Div p) = B.OpDiv p
convertOp (A.Mod p) = B.OpMod p
convertOp (A.And p) = B.OpMul p
convertOp (A.Or p) = B.OpAdd p



convertValueT :: A.Value a -> (B.Val a, B.SType a)
convertValueT (A.Const p (A.IntC p'' val)) = (B.VInt p val, B.Int p)
convertValueT (A.Const p (A.ByteC p'' val)) = (B.VInt p val, B.Int p)
convertValueT (A.Const p (A.StringC p'' label)) = (B.VInt p 0, B.Ref p $ B.Cl p (B.SymIdent "String")) -- FIXME: ?
convertValueT (A.Const p (A.Null p'' t)) = (B.VNull p'' (convertType t), convertType t)
convertValueT (A.Var p n et) = (B.VVal p (convertType et) (nameToValIdent n), convertType et)

convertValue :: A.Value a -> B.Val a
convertValue = fst . convertValueT

todoAddLogic :: LinearConverter (IRConverterEnv a) [B.Instr a]
todoAddLogic = return []

cmpToOp :: A.Cmp a -> B.Op a
cmpToOp (A.Eq p) = B.OpEQU p
cmpToOp (A.Ne p) = B.OpNE p
cmpToOp (A.Lt p) = B.OpLTH p
cmpToOp (A.Gt p) = B.OpGTH p
cmpToOp (A.Le p) = B.OpLE p
cmpToOp (A.Ge p) = B.OpGE p

convertStmtToFIR :: A.Stmt a -> LinearConverter (IRConverterEnv a) [B.Instr a]
convertStmtToFIR (A.VarDecl p vt vn expr) = case expr of
    A.NewObj p' t -> return [B.INew p (nameToValIdent vn) (classType t)]
    A.NewArray p' t dim -> return [B.INewArr p' (nameToValIdent vn) (convertType t) (convertValue dim)]
    A.NewString p' (A.Label p'' n) -> do
        dts <- oStateGet (^. progDatas)
        --liftPipelineOpt $ printLogInfoStr $ "PROG DATAS ARE:\n" ++ (show dts)
        (A.DataString _ strConst _) <- join $ oStateGet (IM.findUsingM (idMapFailure "convertStmtToFIR" (Errors.ILNEDuplicateLabelledData)) (\(A.DataString _ _ (A.Label _ l)) -> l) n . (^. progDatas))
        return [B.INewStr p' (nameToValIdent vn) strConst]
    A.Val p' val -> return [B.ISet p' (nameToValIdent vn) (convertValue val)]
    A.Call p' fnLabel params -> do
        return [B.ICall p (nameToValIdent vn) (B.Call p (convertType vt) (functionName Nothing fnLabel) (map convertValue params) [])]
        -- argTempNames <- mapM (const $ newRetTempName p') params
        -- argTempInstrs <- return $ map (\(tmpName, val) -> B.ISet p (nameToValIdent tmpName) (convertValue val)) $ zip argTempNames params
        -- argParams <- return $ map (\(param, v) -> B.VVal p (snd $ convertValueT param) v) $ zip params $ map nameToValIdent argTempNames
        -- return $ argTempInstrs ++ [B.ICall p (nameToValIdent vn) (B.Call p (convertType vt) (functionName fnLabel) argParams)]
    A.MCall p' _ (A.Label p'' methodName) (A.Label _ clsName) params -> 
        return [B.ICall p (nameToValIdent vn) (B.CallVirt p (convertType vt) (B.QIdent p (B.SymIdent clsName) (B.SymIdent methodName)) (map convertValue $ params))]
        --return [B.ICall p (nameToValIdent vn) (B.Call p (convertType vt) (B.QIdent p (B.SymIdent "~cl_TopLevel") (B.SymIdent methodName)) (map convertValue params))]
    A.ArrAccess p' n v -> todoAddLogic
    A.MemberAccess p' n cls member fieldType -> do 
        return [B.ILoad p (nameToValIdent vn) (B.PFld p ((B.Ref p) $ convertType fieldType) (B.VVal p (classType cls) (nameToValIdent n)) (functionName (Just cls) member) )]
    A.IntToByte p' v -> todoAddLogic
    A.ByteToInt p' v -> todoAddLogic
    A.Not p' v -> return [B.IOp p (nameToValIdent vn) (B.VInt p' 0) (B.OpSub p') (convertValue v)]
    A.BinOp p' op v1 v2 -> return [B.IOp p (nameToValIdent vn) (convertValue v1) (convertOp op) (convertValue v2)]
    A.Cast p' l@(A.Label _ clsName) v ->
        return [B.ICall p (nameToValIdent vn) (B.Call p (convertType $ A.Reference p l) (functionName Nothing $ A.Label p "__cast") [convertValue v] [B.LabIdent $ "_class_" ++ clsName])]
convertStmtToFIR (A.VCall p vt fnLabel params) = do
    return [B.IVCall p (B.Call p (convertType vt) (functionName Nothing fnLabel) (map convertValue params) [])]
--convertStmtToFIR (A.VMCall p vt (A.Name _ clsName) (A.Label _ fnName) params) = do
--    return [B.IVCall p (B.Call p (convertType vt) (B.QIdent p (B.SymIdent clsName) (B.SymIdent fnName)) (map convertValue params))]
convertStmtToFIR (A.Assign p vt (A.Variable p' n) expr) = convertStmtToFIR $ A.VarDecl p vt n expr
convertStmtToFIR (A.Assign p vt (A.Member p' mVal mCls mName) expr) = do 
    exprValName <- newRetTempName p
    exprc <- convertStmtToFIR $ A.VarDecl p vt exprValName expr
    return $ exprc ++ [B.IStore p (B.VVal p (convertType vt) (nameToValIdent exprValName)) (B.PFld p ((B.Ref p) $ convertType vt) (B.VVal p (classType mCls) (nameToValIdent mVal)) (functionName (Just mCls) mName) )]
convertStmtToFIR (A.Assign p vt (A.Array p' _ _) expr) = todoAddLogic
convertStmtToFIR (A.ReturnVal p vt expr) = do
    retName <- newRetTempName p
    exprc <- convertStmtToFIR $ A.VarDecl p vt retName expr
    return $ exprc ++ [B.ISet p (B.ValIdent $ "%v_return") $ B.VVal p (convertType vt) (nameToValIdent retName), B.IJmp p $ B.LabIdent ".L_exit"]
    --return $ exprc ++ [B.IRet p $ B.VVal p (convertType vt) (nameToValIdent retName)]
convertStmtToFIR (A.Return p) = return [B.IJmp p $ B.LabIdent ".L_exit"]
    --return [B.IVRet p]
convertStmtToFIR (A.SetLabel p (A.Label _ l)) = return [B.ILabel p $ B.LabIdent l]
convertStmtToFIR (A.Jump p (A.Label _ l)) = return [B.IJmp p $ B.LabIdent l]
convertStmtToFIR (A.JumpCmp p cmp (A.Label _ l) v1 v2) = do 
    cndName <- newRetTempName p
    return [
        B.IOp p (nameToValIdent cndName) (convertValue v1) (cmpToOp cmp) (convertValue v2)
        , B.ICondJmp p (B.VVal p (B.Bool p) $ nameToValIdent cndName) (B.LabIdent l) (B.LabIdent $ "_easy_" ++ l)
        , B.ILabel p $ B.LabIdent $ "_easy_" ++ l]

--cmpToOp
--ICondJmp a (Val a) LabIdent LabIdent

-- Require:
-- type info at null and references



