module Instant.Backend.LLVM.Compiler where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.List
import qualified Data.Map as M
import Instant.Backend.Base
import Instant.Backend.LLVM.Syntax
import Instant.Syntax
import System.FilePath
import Instant.Backend.LLVM.Entry

data CompilerState = CompilerState
  { 
    variableStoreFreeIndex :: Int,
    errMsg :: String,
    vPrefix :: String,
    rPrefix :: String,
    localVariables :: M.Map String Int
  }

type CompilerM = ExceptT String (State CompilerState)

noCode :: [LLVMOp]
noCode = []

valPrefix = "def_value_"
varPrefix = "def_variable_"

getLocalVar :: CompilerM Int
getLocalVar = gets variableStoreFreeIndex

initialCompilerState :: CompilerState
initialCompilerState = (CompilerState 0 "" valPrefix varPrefix M.empty)

createVariable :: CompilerM String
createVariable = modify (\currentState -> currentState {variableStoreFreeIndex = variableStoreFreeIndex currentState + 1}) *> ((valPrefix <>) . show <$> getLocalVar)

getVariable :: ASTMeta -> String -> CompilerM String
getVariable astMetadata variableName =
  gets ((M.lookup variableName) . localVariables) >>= \case
    Nothing -> throwError $ getMetaDescription astMetadata ++ " Error: Variable is not defined: " ++ variableName
    Just i -> pure $ varPrefix ++ variableName ++ show i

getVariableNoMeta :: String -> CompilerM String
getVariableNoMeta variableName = gets ((M.! variableName) . localVariables) >>= \i -> pure $ varPrefix ++ variableName ++ show i

initVar :: String -> CompilerM String
initVar v =
  gets ((M.lookup v) . localVariables) >>= \case
    Just i ->
      modify (\s -> s {localVariables = M.insert v (i + 1) (localVariables s)}) >> getVariableNoMeta v
    Nothing ->
      modify (\s -> s {localVariables = M.insert v 0 (localVariables s)}) >> initVar v
    
compileIExpr :: IExpr -> CompilerM (LLVMLit, [LLVMOp])
compileIExpr = \case
  IExprMinus _ left right -> compileOperator INSTRSub lint32 left right
  IExprMultiplication _ left right -> compileOperator INSTRMul lint32 left right
  IExprVar astMetadata s -> do
    v <- getVariable astMetadata s
    pure (CONSTReg v, noCode)
  IExprInt _ val -> pure (CONSTInt val, noCode)
  IExprPlus _ left right -> compileOperator INSTRAdd lint32 left right
  IExprDiv _ left right -> compileOperator INSTRDiv lint32 left right
  where
    compileOperator op t a b = do
      (aref, acode) <- compileIExpr a
      (bref, bcode) <- compileIExpr b
      i <- createVariable
      let icode = [OPAssignment i (op t aref bref)]
      pure (CONSTReg i, acode ++ bcode ++ icode)

compileICode :: String -> ICode -> CompilerM String
compileICode fileName code = do
  llcode <-
    (++ [OPDoReturn lint32 (constZero)]) . join
      <$> traverse compileIStatement (statements code)
  pure $ (entry fileName) ++ concat (fmap ((<> "\n") . ("  " <>) . toCode) llcode) ++ "\n}"

emitInstruction :: [LLVMOp] -> LLVMOp -> CompilerM [LLVMOp]
emitInstruction code instr = pure $ code ++ [instr]

emitPrintCall ::(LLVMLit, [LLVMOp]) -> CompilerM [LLVMOp]
emitPrintCall (variableReference, code) = do
  emitInstruction code $ OPDoCall
      lint32
      [ptr lint8, LVARARGS]
      (toCode FNPRINTF)
      [ ( ptr lint8,
          CONSTGetElementPtr (4, lint8) (toCode FNINTPRINT) (lint32, constZero) (lint32, constZero)
        ),
        (lint32, variableReference)
      ]

compileIStatement :: IStatement -> CompilerM [LLVMOp]
compileIStatement (IExpr _ e) = do
    exprCompilerCode <- compileIExpr e
    emitPrintCall $ exprCompilerCode
compileIStatement (IAssignment _ v e) = do
    (variableReference, resultCode) <- compileIExpr e
    ref <- initVar v
    emitInstruction resultCode $ OPAssignment ref (INSTRAdd lint32 variableReference (constZero))