module Instant.Backend.X86.Compiler where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Instant.Backend.X86.Jasmine
import Instant.Backend.X86.Stack
import Instant.Syntax
import qualified Language.JVM.Common as J
import System.FilePath
import Instant.Backend.X86.Entry
import Instant.Backend.Base

type CompilerM = ExceptT String (StateT (Set String) (Reader (Map String Int)))

getVarName :: String -> CompilerM Int
getVarName v = asks (M.! v)

checkVarInit :: ASTMeta -> String -> CompilerM ()
checkVarInit astMetadata v =
  get >>= \s ->
    if S.member v s
      then pure ()
      else throwError $ getMetaDescription astMetadata ++ " Undefined variable " ++ v

registerVar :: String -> CompilerM ()
registerVar v = modify (S.insert v)

comutative :: J.Instruction -> Bool
comutative J.Iadd = True
comutative J.Imul = True
comutative _ = False

compileIExpr :: IExpr -> CompilerM [J.Instruction]
compileIExpr e = ($ []) . snd <$> builder e
  where
    builder :: IExpr -> CompilerM (Int, [J.Instruction] -> [J.Instruction])
    builder = \case
      -- optimizes stack
      IExprInt _ i -> pure (1, ((J.Ldc $ J.Integer $ fromIntegral i) :))
      IExprVar astMetadata v -> do
        checkVarInit astMetadata v
        i <- getVarName v
        pure (1, ((J.Iload $ intToWord16 i) :))
      IExprPlus _ a b -> buildOp J.Iadd a b
      IExprMinus _ a b -> buildOp J.Isub a b
      IExprMultiplication _ a b -> buildOp J.Imul a b
      IExprDiv _ a b -> buildOp J.Idiv a b

    buildOp op a b = do
      (ai, ab) <- builder a
      (bi, bb) <- builder b
      case compare ai bi of
        EQ -> pure (ai + 1, ab . bb . (op :))
        LT -> pure (bi, bb . ab . (if comutative op then id else (J.Swap :)) . (op :))
        GT -> pure (ai, ab . bb . (op :))

compileIStatement :: IStatement -> CompilerM [J.Instruction]
compileIStatement = \case
  IExpr _ e ->
    join
      <$> sequence
        [ pure [J.Getstatic $ J.FieldId {J.fieldIdClass = (J.mkClassName "java/lang/System/out"), J.fieldIdName = "Ljava/io/PrintStream;", J.fieldIdType = J.IntType}],
          compileIExpr e,
          pure [J.Invokevirtual J.IntType J.MethodKey {J.methodKeyName = "java/io/PrintStream/println(I)V", J.methodKeyReturnType = Nothing, J.methodKeyParameterTypes = [J.IntType]}]
        ]
  IAssignment _ v e -> do
    easm <- compileIExpr e
    registerVar v
    idx <- getVarName v
    pure $ easm ++ [J.Istore $ intToWord16 idx]

compileICode :: String -> ICode -> CompilerM String
compileICode fileName code = do
  varsCount <- asks M.size
  jvm <- join <$> mapM compileIStatement (statements code)
  let funBody = unlines . fmap (("  " <>) . toCode) $ jvm
  pure $
    (entry fileName) ++ "\n.method public static main([Ljava/lang/String;)V\n.limit locals " ++ (show (varsCount + 1)) ++ "\n.limit stack " ++ (show (estimateStackSize jvm)) ++ "\n" ++ funBody ++ "return\n.end method\n"
