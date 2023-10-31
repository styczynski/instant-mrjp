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
import Instant.Backend.LLVM.Entry

data CompilerState = CompilerState
  { csStore :: Int,
    csVarMap :: Map String Int
  }

type CompilerM = ExceptT String (State CompilerState)

lastId :: CompilerM Int
lastId = gets csStore

newRef :: CompilerM String
newRef = modify (\s -> s {csStore = csStore s + 1}) *> (("val_" <>) . show <$> lastId)

lookupVarAt :: ASTMeta -> String -> CompilerM String
lookupVarAt ann v =
  gets ((M.lookup v) . csVarMap) >>= \case
    Nothing -> throwError $ at ann ++ " Undefined variable " ++ v
    Just i -> pure ("var_" ++ v ++ show i)

lookupVar :: String -> CompilerM String
lookupVar v = gets ((M.! v) . csVarMap) >>= \i -> pure ("var_" ++ v ++ show i)

initVar :: String -> CompilerM String
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
  CompilerM (LLVMLit, LLVM)
compileOperator op t a b = do
  (aref, acode) <- compileIExpr a
  (bref, bcode) <- compileIExpr b
  i <- newRef
  let icode = [OPAssignment i (op t aref bref)]
  pure (CONSTReg i, acode ++ bcode ++ icode)

compileIExpr :: IExpr -> CompilerM (LLVMLit, LLVM)
compileIExpr = \case
  IExprInt _ i -> pure (CONSTInt i, [])
  IExprVar ann s -> do
    v <- lookupVarAt ann s
    pure (CONSTReg v, [])
  IExprPlus _ a b -> compileOperator INSTRAdd lint32 a b
  IExprMinus _ a b -> compileOperator INSTRSub lint32 a b
  IExprMultiplication _ a b -> compileOperator INSTRMul lint32 a b
  IExprDiv _ a b -> compileOperator INSTRDiv lint32 a b

compileIStatement :: IStatement -> CompilerM LLVM
compileIStatement = \case
  IAssignment _ v e -> do
    (valRef, ecode) <- compileIExpr e
    ref <- initVar v
    pure $ ecode ++ [OPAssignment ref (INSTRAdd lint32 valRef (CONSTInt 0))]
  IExpr _ e -> do
    (valRef, ecode) <- compileIExpr e
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

compileICode :: String -> ICode -> CompilerM String
compileICode fileName code = do
  llcode <-
    (++ [OPDoReturn lint32 (CONSTInt 0)]) . join
      <$> traverse compileIStatement (statements code)
  pure $ (entry fileName) ++ concat (fmap ((<> "\n") . ("  " <>) . toCode) llcode) ++ "\n}"
