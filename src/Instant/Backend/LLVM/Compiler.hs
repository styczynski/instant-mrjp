module Instant.Backend.LLVM.Compiler where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Instant.Backend.Base
import Instant.Backend.LLVM.Syntax
import Instant.Syntax
import System.FilePath

data CompilerState = CompilerState
  { csStore :: Int,
    csVarMap :: Map String Int
  }

type LLVMCompiler = ExceptT String (State CompilerState)

lastId :: LLVMCompiler Int
lastId = gets csStore

newRef :: LLVMCompiler String
newRef = modify (\s -> s {csStore = csStore s + 1}) *> (("val_" <>) . show <$> lastId)

lookupVarAt :: ASTMeta -> String -> LLVMCompiler String
lookupVarAt ann v =
  gets ((M.lookup v) . csVarMap) >>= \case
    Nothing -> throwError $ at ann ++ " Undefined variable " ++ v
    Just i -> pure ("var_" ++ v ++ show i)

lookupVar :: String -> LLVMCompiler String
lookupVar v = gets ((M.! v) . csVarMap) >>= \i -> pure ("var_" ++ v ++ show i)

initVar :: String -> LLVMCompiler String
initVar v =
  gets ((M.lookup v) . csVarMap) >>= \case
    Nothing ->
      modify (\s -> s {csVarMap = M.insert v 0 (csVarMap s)}) >> initVar v
    Just i ->
      modify (\s -> s {csVarMap = M.insert v (i + 1) (csVarMap s)}) >> lookupVar v

compileOperator ::
  (LLVMType -> LLVMLit -> LLVMLit -> LLVMExpr) ->
  LLVMType ->
  IExpr ->
  IExpr ->
  LLVMCompiler (LLVMLit, LLVM)
compileOperator op t a b = do
  (aref, acode) <- compileExpr a
  (bref, bcode) <- compileExpr b
  i <- newRef
  let icode = [OPAssignment i (op t aref bref)]
  pure (CONSTReg i, acode ++ bcode ++ icode)

compileExpr :: IExpr -> LLVMCompiler (LLVMLit, LLVM)
compileExpr = \case
  IExprInt _ i -> pure (CONSTInt i, [])
  IExprVar ann s -> do
    v <- lookupVarAt ann s
    pure (CONSTReg v, [])
  IExprPlus _ a b -> compileOperator INSTRAdd lint32 a b
  IExprMinus _ a b -> compileOperator INSTRSub lint32 a b
  IExprMultiplication _ a b -> compileOperator INSTRMul lint32 a b
  IExprDiv _ a b -> compileOperator INSTRDiv lint32 a b

compileStmt :: IStatement -> LLVMCompiler LLVM
compileStmt = \case
  IAssignment _ v e -> do
    (valRef, ecode) <- compileExpr e
    ref <- initVar v
    pure $ ecode ++ [OPAssignment ref (INSTRAdd lint32 valRef (CONSTInt 0))]
  IExpr _ e -> do
    (valRef, ecode) <- compileExpr e
    let call =
          OPDoCall
            lint32
            [ptr lint8, LVARARGS]
            "@printf"
            [ ( ptr lint8,
                CONSTGetElementPtr (4, lint8) "@.intprint" (lint32, CONSTInt 0) (lint32, CONSTInt 0)
              ),
              (lint32, valRef)
            ]
    pure $ ecode ++ [call]

invocation :: String
invocation =
  unlines
    [ "@.intprint = private unnamed_addr constant [4 x lint8] c\"%d\\0A\\00\", align 1",
      "declare dso_local lint32 @printf(lint8*, ...) #1",
      "",
      "define dso_local lint32 @main() {\n"
    ]

compileInstant :: ICode -> LLVMCompiler String
compileInstant code = do
  llcode <-
    (++ [OPDoReturn lint32 (CONSTInt 0)]) . join
      <$> traverse compileStmt (statements code)
  pure $ invocation ++ concat (fmap ((<> "\n") . ("  " <>) . toCode) llcode) ++ "\n}"
