
module Instant.Backend.LLVM.Compiler where

import           Data.List
import           Data.Map(Map)
import qualified Data.Map as M
import           Control.Monad.State.Strict
import           Control.Monad.Except
import           System.FilePath

import Instant.Syntax
import Instant.Backend.Base

import Instant.Backend.LLVM.Syntax

data CompilerState = CompilerState
  { csStore :: Int
  , csVarMap :: Map String Int
  }


type LLVMCompiler = ExceptT String (State CompilerState)


lastId :: LLVMCompiler Int
lastId = gets csStore


newRef :: LLVMCompiler String
newRef = modify (\s -> s{csStore = csStore s + 1}) *> (("val_"<>) . show <$> lastId)


lookupVarAt :: ASTMeta -> String -> LLVMCompiler String
lookupVarAt ann v = gets ((M.lookup v) . csVarMap) >>= \case
  Nothing -> throwError $ at ann ++ " Undefined variable " ++ v
  Just i -> pure ("var_" ++ v ++ show i)


lookupVar :: String -> LLVMCompiler String
lookupVar v = gets ((M.! v) . csVarMap) >>= \i -> pure ("var_" ++ v ++ show i)


initVar :: String -> LLVMCompiler String
initVar v = gets ((M.lookup v) . csVarMap) >>= \case
  Nothing ->
    modify (\s -> s{csVarMap = M.insert v 0 (csVarMap s)}) >> initVar v
  Just i ->
    modify (\s -> s{csVarMap = M.insert v (i + 1) (csVarMap s)}) >> lookupVar v


compileOperator :: (LLVMType -> LLVMLit -> LLVMLit -> LLVMExpr)
                -> LLVMType -> IExpr -> IExpr
                -> LLVMCompiler (LLVMLit, LLVM)
compileOperator op t a b = do
  (aref, acode) <- compileExpr a
  (bref, bcode) <- compileExpr b
  i <- newRef
  let icode = [LLOAssg i (op t aref bref)]
  pure (LLLReg i, acode ++ bcode ++ icode)


compileExpr :: IExpr -> LLVMCompiler (LLVMLit, LLVM)
compileExpr = \case
  IExprInt _ i -> pure (LLLInt i, [])
  IExprVar ann s -> do
    v <- lookupVarAt ann s
    pure (LLLReg v, [])
  IExprPlus _ a b -> compileOperator LLEAdd i32 a b
  IExprMinus _ a b -> compileOperator LLESub i32 a b
  IExprMultiplication _ a b -> compileOperator LLEMul i32 a b
  IExprDiv _ a b -> compileOperator LLIExprDiv i32 a b


compileStmt :: IStatement -> LLVMCompiler LLVM
compileStmt = \case
  IAssignment _ v e -> do
    (valRef, ecode) <- compileExpr e
    ref <- initVar v
    pure $ ecode ++ [LLOAssg ref (LLEAdd i32 valRef (LLLInt 0))]
  IExpr _ e -> do
    (valRef, ecode) <- compileExpr e
    let call =
          LLOCall i32 [ptr i8, LLTVargs] "@printf"
          [ ( ptr i8
            , LLLGetElementPtr (4, i8) "@.intprint" (i32, LLLInt 0) (i32, LLLInt 0)
            )
          , (i32, valRef)
          ]
    pure $ ecode ++ [call]


invocation :: String
invocation = unlines
  [ "@.intprint = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1"
  , "declare dso_local i32 @printf(i8*, ...) #1"
  , ""
  , "define dso_local i32 @main() {\n"
  ]


compileInstant :: ICode -> LLVMCompiler String
compileInstant code = do
  llcode <- (++[LLORet i32 (LLLInt 0)]) . join <$>
    traverse compileStmt (statements code)
  pure $ invocation ++ concat (fmap ((<>"\n") . ("  "<>) . serializeOp) llcode) ++ "\n}"

