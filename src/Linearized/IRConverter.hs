{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Linearized.IRConverter(initialState, run) where

import Control.Lens
import Control.DeepSeq
import Control.Monad
import GHC.Generics (Generic)

import IR.Utils
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
    B.MthdDef p "" (B.FType p rt $ map (\(B.Param _ t _) -> t) params) l

run :: (A.Program a) -> LinearConverter (IRConverterEnv a) (B.Program a)
run prog@(A.Program p structs fns datas) = do
    oStateSet (\env -> env & progDatas .~ datas)
    liftPipelineOpt $ printLogInfoStr $ "AST to convert:\n" ++ (show prog)
    classDefs <- IM.mapListM (\_ struct -> convertStructureToFIR struct) structs
    methodsFns <- IM.mapListM (\_ fn -> convertFunctionToFIR fn) fns
    builtinFns <- return $ map (\(A.Fun _ cls l rt args stmts) -> B.MthdDef p "" (B.FType p (convertType rt) $ map (convertType . fst) args) (functionName cls l)) $ map (fmap (const p)) builtIns
    globalClassDef <- return $ B.ClDef p (B.IRTargetRefName "~cl_TopLevel") [] [] (builtinFns ++ map extractMethodDef methodsFns)
    return $ B.Program p (B.Meta p $ classDefs ++ [globalClassDef]) methodsFns

convertStructureToFIR :: (A.Structure a) -> LinearConverter (IRConverterEnv a) (B.ClassDef a)
convertStructureToFIR (A.Struct p (A.Label _ name) chain methods fields) = do
    --(A.Label fieldPos fieldName), fieldType, _
    fields <- return $ IM.mapList (\_ (A.Field fieldPos fieldType (A.Label _ fieldName)) -> B.FldDef fieldPos (convertType fieldType) $ B.IRTargetRefName fieldName) fields
    methods <- return $ IM.mapList (\_ (A.Method methodPos (A.Label _ methodParent) methodCls methodName methodType methodArgs) -> B.MthdDef methodPos methodParent (B.FType methodPos (convertType methodType) (map (convertType . fst) methodArgs)) (functionName (Just methodCls) methodName)) methods
    return $ B.ClDef p (B.IRTargetRefName name) (map (\(A.Label _ pCls) -> B.IRTargetRefName pCls) chain) fields methods

convertFunctionToFIR :: (A.Function a) -> LinearConverter (IRConverterEnv a) (B.Method a)
convertFunctionToFIR (A.Fun p cls l rt args stmts) = do
    instrs <- return . concat =<< mapM convertStmtToFIR stmts
    params <- return $ map (\(t, n) -> B.Param p (convertType t) (argNameToIRValueName n)) args
    -- forM_ (zip argDecls [0..]) (\((_, val), i) -> emit $ ILoad () (valName val) (PParam () (Ref () $ valType_ val) i (valName val)))
    --B.ISet p (nameToIRValueName n) (B.VVal p (convertType t) $ (argNameToIRValueName n))
    paramsMoves <- return $ map (\((t, n), i) -> B.ILoad p (nameToIRValueName n) (B.PParam p ((B.Ref p) $ convertType t) i (argNameToIRValueName n))) $ zip args [0..]
    let instrs' = fixpointBy (fmap $ const ()) (\ins -> let (ins', trimmedLabel) = trimLabel ins in removeDoubleJumps $ maybe (ins') (\l -> replaceLabels l ".L_exit" ins') trimmedLabel) instrs
    --let (instrs', trimmedLabel) = trimLabel instrs
    --let instrs'' = maybe (instrs') (\l -> removeDoubleJumps $ replaceLabels l ".L_exit" instrs') trimmedLabel
    return $ B.Mthd p (convertType rt) (functionName cls l) params $ [B.ILabel p (B.IRLabelName ".L_entry")] ++ paramsMoves ++ instrs' ++ [B.ILabel p $ B.IRLabelName ".L_exit", B.IRet p $ B.VVal p (convertType rt) $ B.IRValueName $ "%v_return"]

removeDoubleJumps :: [B.Instr a] -> [B.Instr a]
removeDoubleJumps (j1@(B.IJmp  p (B.IRLabelName l)) : (B.IJmp _ (B.IRLabelName _)) : rest) = j1 : removeDoubleJumps rest
removeDoubleJumps (j1@(B.ICondJmp p v (B.IRLabelName l1) (B.IRLabelName l2)) : (B.IJmp _ (B.IRLabelName _)) : rest) = j1 : removeDoubleJumps rest
removeDoubleJumps (j1@(B.IJmp  p (B.IRLabelName l)) : (B.ICondJmp _ _ _ _) : rest) = j1 : removeDoubleJumps rest
removeDoubleJumps (j1@(B.ICondJmp p v (B.IRLabelName l1) (B.IRLabelName l2)) : (B.ICondJmp _ _ _ _) : rest) = j1 : removeDoubleJumps rest
removeDoubleJumps (instr : rest) = instr : removeDoubleJumps rest
removeDoubleJumps [] = []

replaceLabels :: String -> String -> [B.Instr a] -> [B.Instr a]
replaceLabels lFind lReplace ((B.IJmp  p (B.IRLabelName l)) : rest) | l == lFind = (B.IJmp p (B.IRLabelName lReplace)) : replaceLabels lFind lReplace rest
replaceLabels lFind lReplace ((B.ICondJmp p v (B.IRLabelName l1) (B.IRLabelName l2)) : rest) | (l1 == lFind || l2 == lFind) =
    let l1' = if l1 == lFind then lReplace else l1 in
    let l2' = if l2 == lFind then lReplace else l2 in
    (B.ICondJmp p v (B.IRLabelName l1') (B.IRLabelName l2')) : replaceLabels lFind lReplace rest
replaceLabels lFind lReplace ((B.ILabel p (B.IRLabelName l)) : rest) | l == lFind = replaceLabels lFind lReplace rest
replaceLabels lFind lReplace (instr : rest) = instr : replaceLabels lFind lReplace rest
replaceLabels _ _ [] = []

trimLabel :: [B.Instr a] -> ([B.Instr a], Maybe String)
trimLabel instrs = let (instrs', rem) = trimLabels' $ reverse instrs in (reverse instrs', rem)
    where
        trimLabels' (B.ILabel _ (B.IRLabelName l) : rest) = (rest, Just l)
        trimLabels' rest = (rest, Nothing)

--Mthd a (SType a) (QIdent a) [Param a] [Instr a]
--Fun a (Label a) (Type a) [{-args-}(Type a, Name a)] [Stmt a]

newLabel :: a -> String -> LinearConverter (IRConverterEnv a) (B.IRLabelName)
newLabel p prefix = do
    tvnc <- oStateGet (^. tempVarNameCounter)
    oStateSet (\env -> env & tempVarNameCounter %~ (+1))
    return $ B.IRLabelName $ "_LT_" ++ prefix ++ (show tvnc)

newRetTempName :: a -> LinearConverter (IRConverterEnv a) (A.Name a)
newRetTempName p = do
    tvnc <- oStateGet (^. tempVarNameCounter)
    oStateSet (\env -> env & tempVarNameCounter %~ (+1))
    return $ A.Name p $ "__temp_" ++ (show tvnc)

convertType :: A.Type a -> B.SType a
convertType (A.IntT p) = B.Int p
convertType (A.ByteT p) = B.Bool p
convertType (A.Reference p (A.Label _ clname)) = B.Ref p $ B.Cl p $ B.IRTargetRefName clname
convertType (A.ArrT p t) =  B.Ref p $ B.Arr p (convertType t)

functionName :: (Maybe (A.Label a)) -> A.Label a -> B.QIdent a
functionName Nothing (A.Label p name) = B.QIdent p (B.IRTargetRefName "~cl_TopLevel") (B.IRTargetRefName name)
functionName (Just (A.Label _ cls)) (A.Label p name) = B.QIdent p (B.IRTargetRefName cls) (B.IRTargetRefName name)

labelToIRValueName :: A.Label a -> B.IRValueName
labelToIRValueName (A.Label _ name) = B.IRValueName name

nameToIRValueName :: A.Name a -> B.IRValueName
nameToIRValueName (A.Name _ name) = B.IRValueName $ "%v_" ++ name

argNameToIRValueName :: A.Name a -> B.IRValueName
argNameToIRValueName (A.Name _ name) = B.IRValueName $ "%a_" ++ name


classType :: A.Label a -> B.SType a
classType (A.Label p name) = B.Cl p $ B.IRTargetRefName name

convertOp :: A.Op a -> B.Op a 
convertOp (A.Add p) = B.OpAdd p
convertOp (A.Sub p) = B.OpSub p
convertOp (A.Mul p) = B.OpMul p
convertOp (A.Div p) = B.OpDiv p
convertOp (A.Mod p) = B.OpMod p
convertOp _ = error "Invalid operation in convertOp"
-- convertOp (A.And p) = B.OpMul p
-- convertOp (A.Or p) = B.OpAdd p



convertValueT :: A.Value a -> (B.Val a, B.SType a)
convertValueT (A.Const p (A.IntC p'' val)) = (B.VInt p val, B.Int p)
convertValueT (A.Const p (A.ByteC p'' val)) = (B.VInt p val, B.Int p)
convertValueT (A.Const p (A.StringC p'' label)) = (B.VInt p 0, B.Ref p $ B.Cl p (B.IRTargetRefName "String")) -- FIXME: ?
convertValueT (A.Const p (A.Null p'' t)) = (B.VNull p'' (convertType t), convertType t)
convertValueT (A.Var p n et) = (B.VVal p (convertType et) (nameToIRValueName n), convertType et)

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
    A.NewObj p' t -> return [B.INew p (nameToIRValueName vn) (classType t)]
    A.NewArray p' t dim -> return [B.INewArr p' (nameToIRValueName vn) (convertType t) (convertValue dim)]
    A.NewString p' (A.Label p'' n) -> do
        dts <- oStateGet (^. progDatas)
        --liftPipelineOpt $ printLogInfoStr $ "PROG DATAS ARE:\n" ++ (show dts)
        (A.DataString _ strConst _) <- join $ oStateGet (IM.findUsingM (idMapFailure "convertStmtToFIR" (Errors.ILNEDuplicateLabelledData)) (\(A.DataString _ _ (A.Label _ l)) -> l) n . (^. progDatas))
        return [B.INewStr p' (nameToIRValueName vn) strConst]
    A.Val p' val -> return [B.ISet p' (nameToIRValueName vn) (convertValue val)]
    A.Call p' fnLabel params -> do
        return [B.ICall p (nameToIRValueName vn) (B.Call p (convertType vt) (functionName Nothing fnLabel) (map convertValue params) [])]
        -- argTempNames <- mapM (const $ newRetTempName p') params
        -- argTempInstrs <- return $ map (\(tmpName, val) -> B.ISet p (nameToIRValueName tmpName) (convertValue val)) $ zip argTempNames params
        -- argParams <- return $ map (\(param, v) -> B.VVal p (snd $ convertValueT param) v) $ zip params $ map nameToIRValueName argTempNames
        -- return $ argTempInstrs ++ [B.ICall p (nameToIRValueName vn) (B.Call p (convertType vt) (functionName fnLabel) argParams)]
    A.MCall p' _ (A.Label p'' methodName) (A.Label _ clsName) params -> 
        return [B.ICall p (nameToIRValueName vn) (B.CallVirt p (convertType vt) (B.QIdent p (B.IRTargetRefName clsName) (B.IRTargetRefName methodName)) (map convertValue $ params))]
        --return [B.ICall p (nameToIRValueName vn) (B.Call p (convertType vt) (B.QIdent p (B.IRTargetRefName "~cl_TopLevel") (B.IRTargetRefName methodName)) (map convertValue params))]
    A.ArrAccess p' n v ->
        return [B.ILoad p (nameToIRValueName vn) (B.PElem p ((B.Ref p) $ convertType vt) (B.VVal p (B.Ref p $ B.Arr p (convertType vt)) (nameToIRValueName n)) (convertValue v))]
    A.MemberAccess p' n cls member fieldType -> do 
        return [B.ILoad p (nameToIRValueName vn) (B.PFld p ((B.Ref p) $ convertType fieldType) (B.VVal p (classType cls) (nameToIRValueName n)) (functionName (Just cls) member) )]
    A.IntToByte p' v -> todoAddLogic
    A.ByteToInt p' v -> todoAddLogic
    A.BinOp p' (A.And p'') v1 v2 -> do
        --boolVal <- newRetTempName p''
        lnext <- newLabel p' "ANEXT"
        lend <- newLabel p' "AEND"
        ltrue <- newLabel p' "ATRUE"
        return [
            B.ISet p' (nameToIRValueName vn) (B.VFalse p'')
            , B.ICondJmp p (convertValue v1) lnext lend
            , B.ILabel p lnext
            , B.ICondJmp p (convertValue v2) ltrue lend
            , B.ILabel p ltrue
            , B.ISet p' (nameToIRValueName vn) (B.VTrue p'')
            , B.IJmp p lend
            , B.ILabel p lend]
    A.BinOp p' (A.Or p'') v1 v2 -> do
        --boolVal <- newRetTempName p''
        lnext <- newLabel p' "ONEXT"
        lend <- newLabel p' "OEND"
        lfalse <- newLabel p' "OFALSE"
        return [
            B.ISet p' (nameToIRValueName vn) (B.VTrue p'')
            , B.ICondJmp p (convertValue v1) lend lnext
            , B.ILabel p lnext
            , B.ICondJmp p (convertValue v2) lend lfalse
            , B.ILabel p lfalse
            , B.ISet p' (nameToIRValueName vn) (B.VFalse p'')
            , B.IJmp p lend
            , B.ILabel p lend]
    A.Not p' v -> do --return [B.IOp p (nameToIRValueName vn) (B.VInt p' 0) (B.OpSub p') (convertValue v)]
        lend <- newLabel p' "NEND"
        lfalse <- newLabel p' "NFALSE"
        return [
            B.ISet p' (nameToIRValueName vn) (B.VTrue p')
            , B.ICondJmp p (convertValue v) lfalse lend
            , B.ILabel p lfalse
            , B.ISet p' (nameToIRValueName vn) (B.VFalse p')
            , B.IJmp p lend
            , B.ILabel p lend]
    A.BinOp p' op v1 v2 -> return [B.IOp p (nameToIRValueName vn) (convertValue v1) (convertOp op) (convertValue v2)]
    A.Cast p' l@(A.Label _ clsName) v ->
        return [B.ICall p (nameToIRValueName vn) (B.Call p (convertType $ A.Reference p l) (functionName Nothing $ A.Label p "__cast") [convertValue v] [B.IRLabelName $ "_class_" ++ clsName])]
convertStmtToFIR (A.IncrCounter p t n val) = do
    gcCalls <- if val < 0 then return [B.IVCall p (B.Call p (convertType t) (functionName Nothing $ A.Label p "run_gc") [] [])] else return []
    return $ gcCalls ++ [B.IAddRef p (convertType t) (convertValue $ A.Var p n t) val]
-- convertStmtToFIR (A.DecrCounter p t n) =
--     return [B.IAddRef p (convertType t) (convertValue $ A.Var p n t) (-1)]
convertStmtToFIR (A.VCall p vt fnLabel params) = do
    return [B.IVCall p (B.Call p (convertType vt) (functionName Nothing fnLabel) (map convertValue params) [])]
convertStmtToFIR (A.VMCall p vt _ (A.Label _ methodName) (A.Label _ clsName) params) = do
    return [B.IVCall p (B.CallVirt p (convertType vt) (B.QIdent p (B.IRTargetRefName clsName) (B.IRTargetRefName methodName)) (map convertValue $ params))]
convertStmtToFIR (A.Assign p vt (A.Variable p' n) expr) = convertStmtToFIR $ A.VarDecl p vt n expr
convertStmtToFIR (A.Assign p vt (A.Member p' mVal mCls mName) expr) = do 
    exprValName <- newRetTempName p
    exprc <- convertStmtToFIR $ A.VarDecl p vt exprValName expr
    return $ exprc ++ [B.IStore p (B.VVal p (convertType vt) (nameToIRValueName exprValName)) (B.PFld p ((B.Ref p) $ convertType vt) (B.VVal p (classType mCls) (nameToIRValueName mVal)) (functionName (Just mCls) mName) )]
convertStmtToFIR (A.Assign p vt (A.Array p' n v) expr) = do
    exprValName <- newRetTempName p
    exprc <- convertStmtToFIR $ A.VarDecl p vt exprValName expr
    return $ exprc ++ [B.IStore p (B.VVal p (convertType vt) (nameToIRValueName exprValName)) (B.PElem p ((B.Ref p) $ convertType vt) (B.VVal p (B.Ref p $ B.Arr p (convertType vt)) (nameToIRValueName n)) (convertValue v))]
convertStmtToFIR (A.ReturnVal p vt expr) = do
    retName <- newRetTempName p
    exprc <- convertStmtToFIR $ A.VarDecl p vt retName expr
    return $ exprc ++ [B.ISet p (B.IRValueName $ "%v_return") $ B.VVal p (convertType vt) (nameToIRValueName retName), B.IJmp p $ B.IRLabelName ".L_exit"]
    --return $ exprc ++ [B.IRet p $ B.VVal p (convertType vt) (nameToIRValueName retName)]
convertStmtToFIR (A.Return p) = return [B.IJmp p $ B.IRLabelName ".L_exit"]
    --return [B.IVRet p]
convertStmtToFIR (A.SetLabel p (A.Label _ l)) = return [B.ILabel p $ B.IRLabelName l]
convertStmtToFIR (A.Jump p (A.Label _ l)) = return [B.IJmp p $ B.IRLabelName l]
convertStmtToFIR (A.JumpCmp p cmp (A.Label _ l) (A.Label _ lpass) v1 v2) = do 
    cndName <- newRetTempName p
    return [
        B.IOp p (nameToIRValueName cndName) (convertValue v1) (cmpToOp cmp) (convertValue v2)
        , B.ICondJmp p (B.VVal p (B.Bool p) $ nameToIRValueName cndName) (B.IRLabelName l) (B.IRLabelName lpass)]
    -- return [
    --     B.IOp p (nameToIRValueName cndName) (convertValue v1) (cmpToOp cmp) (convertValue v2)
    --     , B.ICondJmp p (B.VVal p (B.Bool p) $ nameToIRValueName cndName) (B.IRLabelName l) (B.IRLabelName $ "_easy_" ++ l)
    --     , B.ILabel p $ B.IRLabelName $ "_easy_" ++ l]
convertStmtToFIR stmt = return $ error $ "Unhandled stmt in convertStmtToFIR: " ++ (show stmt)
--cmpToOp
--ICondJmp a (Val a) IRLabelName IRLabelName

-- Require:
-- type info at null and references



