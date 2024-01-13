{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Linearized.IRConverter(initialState, run) where

import Control.Lens
import Control.DeepSeq
import GHC.Generics (Generic)

import qualified Utils.Containers.IDMap as IM

import qualified IR.Syntax.Syntax as B
import qualified Linearized.Syntax as A

import Reporting.Logs

import Linearized.Def

data IRConverterEnv = IRConverterEnv
  {
    _ircvTempVarNameCounter :: Int
  } deriving (Show, Generic, NFData)

makeLensesWith abbreviatedFields ''IRConverterEnv

initialState :: IRConverterEnv
initialState = IRConverterEnv {
    _ircvTempVarNameCounter = 0
}

extractMethodDef :: (B.Method a) -> (B.MethodDef a)
extractMethodDef (B.Mthd p rt l params _) = 
    B.MthdDef p (B.FType p rt $ map (\(B.Param _ t _) -> t) params) l

run :: (A.Program a) -> LinearConverter IRConverterEnv (B.Program a)
run prog@(A.Program p structs fns datas) = do
    liftPipelineOpt $ printLogInfoStr $ "AST to convert:\n" ++ (show prog)
    classDefs <- return $ []
    methodsFns <- IM.mapListM (\_ fn -> convertFunctionToFIR fn) fns
    globalClassDef <- return $ B.ClDef p (B.SymIdent "~cl_TopLevel") [] (map extractMethodDef methodsFns)
    return $ B.Program p (B.Meta p $ classDefs ++ [globalClassDef]) methodsFns

convertFunctionToFIR :: (A.Function a) -> LinearConverter IRConverterEnv (B.Method a)
convertFunctionToFIR (A.Fun p l rt args stmts) = do
    instrs <- return . concat =<< mapM convertStmtToFIR stmts
    params <- return $ map (\(t, n) -> B.Param p (convertType rt) (nameToValIdent n)) args
    return $ B.Mthd p (convertType rt) (functionName l) params $ [B.ILabel p $ B.LabIdent ".L_entry"] ++ instrs
--Mthd a (SType a) (QIdent a) [Param a] [Instr a]
--Fun a (Label a) (Type a) [{-args-}(Type a, Name a)] [Stmt a]

newRetTempName :: a -> LinearConverter IRConverterEnv (A.Name a)
newRetTempName p = do
    tvnc <- oStateGet (^. tempVarNameCounter)
    oStateSet (\env -> env & tempVarNameCounter %~ (+1))
    return $ A.Name p $ "__temp_" ++ (show tvnc)

convertType :: A.Type a -> B.SType a
convertType (A.IntT p) = B.Int p
convertType (A.ByteT p) = B.Int p
convertType (A.Reference p (A.Label _ clname)) = B.Cl p $ B.SymIdent clname
convertType (A.ArrT p t) = B.Arr p (convertType t)

functionName :: A.Label a -> B.QIdent a
functionName (A.Label p name) = B.QIdent p (B.SymIdent "~cl_TopLevel") (B.SymIdent name)

labelToValIdent :: A.Label a -> B.ValIdent
labelToValIdent (A.Label _ name) = B.ValIdent name

nameToValIdent :: A.Name a -> B.ValIdent
nameToValIdent (A.Name _ name) = B.ValIdent $ "%v_" ++ name

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

convertValue :: A.Value a -> B.Val a
convertValue (A.Const p (A.IntC p'' val)) = B.VInt p val
convertValue (A.Const p (A.ByteC p'' val)) = B.VInt p val
convertValue (A.Const p (A.StringC p'' _)) = B.VInt p 0 -- FIXME: ?
convertValue (A.Const p (A.Null p'' t)) = B.VNull p'' (convertType t)
convertValue (A.Var p n et) = B.VVal p (convertType et) (nameToValIdent n)

todoAddLogic :: LinearConverter IRConverterEnv [B.Instr a]
todoAddLogic = return []

cmpToOp :: A.Cmp a -> B.Op a
cmpToOp (A.Eq p) = B.OpEQU p
cmpToOp (A.Ne p) = B.OpNE p
cmpToOp (A.Lt p) = B.OpLTH p
cmpToOp (A.Gt p) = B.OpGTH p
cmpToOp (A.Le p) = B.OpLE p
cmpToOp (A.Ge p) = B.OpGE p

convertStmtToFIR :: A.Stmt a -> LinearConverter IRConverterEnv [B.Instr a]
convertStmtToFIR (A.VarDecl p vt vn expr) = case expr of
    A.NewObj p' t -> return [B.INew p (nameToValIdent vn) (classType t)]
    A.NewArray p' t dim -> return [B.INewArr p' (nameToValIdent vn) (convertType t) (convertValue dim)]
    A.NewString p' (A.Label p'' n) -> return [B.INewStr p' (nameToValIdent vn) n]
    A.Val p' val -> return [B.ISet p' (nameToValIdent vn) (convertValue val)]
    A.Call p' fnLabel params -> return [B.ICall p (nameToValIdent vn) (B.Call p (convertType vt) (functionName fnLabel) (map convertValue params))]
    A.MCall p' (A.Name _ clsName) (A.Label _ fnName) params -> return [B.ICall p (nameToValIdent vn) (B.Call p (convertType vt) (B.QIdent p (B.SymIdent clsName) (B.SymIdent fnName)) (map convertValue params))]
    A.ArrAccess p' n v -> todoAddLogic
    A.MemberAccess p' n o -> todoAddLogic
    A.IntToByte p' v -> todoAddLogic
    A.ByteToInt p' v -> todoAddLogic
    A.Not p' v -> return [B.IOp p (nameToValIdent vn) (B.VInt p' 0) (B.OpSub p') (convertValue v)]
    A.BinOp p' op v1 v2 -> return [B.IOp p (nameToValIdent vn) (convertValue v1) (convertOp op) (convertValue v2)]
    A.Cast p' l v -> todoAddLogic
convertStmtToFIR (A.Assign p vt (A.Variable p' n) expr) = convertStmtToFIR $ A.VarDecl p vt n expr
convertStmtToFIR (A.Assign p vt (A.Member p' _ _) expr) = todoAddLogic
convertStmtToFIR (A.Assign p vt (A.Array p' _ _) expr) = todoAddLogic
convertStmtToFIR (A.ReturnVal p vt expr) = do
    retName <- newRetTempName p
    exprc <- convertStmtToFIR $ A.VarDecl p vt retName expr
    return $ exprc ++ [B.IRet p $ B.VVal p (convertType vt) (nameToValIdent retName)]
convertStmtToFIR (A.Return p) = return [B.IVRet p]
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



