
module Instant.Backend.JVM.Compiler where

import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Except
import qualified Data.Map as M
import           Data.Map(Map)
import qualified Data.Set as S
import           Data.Set(Set)
import           System.FilePath

import Instant.Syntax
import Instant.Backend.JVM.Jasmine
import Instant.Backend.JVM.Stack

import qualified Language.JVM.Common as J

invocation :: String -> String
invocation filename = unlines
  [ ".source " ++ filename
  , ".class public " ++ takeBaseName filename
  , ".super java/lang/Object"
  , ""
  , ".method public <init>()V"
  , "  aload_0"
  , "  invokenonvirtual java/lang/Object/<init>()V"
  , "  return"
  , ".end method"
  ]


type JVMCompiler = ExceptT String (StateT (Set String) (Reader (Map String Int)))


getVarName :: String -> JVMCompiler Int
getVarName v = asks (M.! v)


checkVarInit :: ASTMeta -> String -> JVMCompiler ()
checkVarInit ann v =
  get >>= \s -> if S.member v s
                then pure ()
                else throwError $ at ann ++ " Undefined variable " ++ v


registerVar :: String -> JVMCompiler ()
registerVar v = modify (S.insert v)


comutative :: J.Instruction -> Bool
comutative J.Iadd = True
comutative J.Imul = True
comutative _   = False


compileExpr :: IExpr -> JVMCompiler [J.Instruction]
compileExpr e = ($[]) . snd <$> builder e where
  builder :: IExpr -> JVMCompiler (Int, [J.Instruction] -> [J.Instruction])
  builder = \case -- optimizes stack
    IExprInt _ i     -> pure (1, ((J.Ldc $ J.Integer $ fromIntegral i):))
    IExprVar ann v   -> do
      checkVarInit ann v
      i <- getVarName v
      pure (1, ((J.Iload $ intToWord16 i):))
    IExprPlus _ a b  -> buildOp J.Iadd a b
    IExprMinus _ a b -> buildOp J.Isub a b
    IExprMultiplication _ a b  -> buildOp J.Imul a b
    IExprDiv _ a b   -> buildOp J.Idiv a b

  buildOp op a b = do
      (ai, ab) <- builder a
      (bi, bb) <- builder b
      case compare ai bi of
        EQ -> pure (ai + 1, ab . bb . (op:))
        LT -> pure (bi    , bb . ab . (if comutative op then id else (J.Swap:)) . (op:))
        GT -> pure (ai    , ab . bb . (op:))


compileStmt :: IStatement -> JVMCompiler [J.Instruction]
compileStmt = \case
  IExpr _ e -> join <$> sequence
    [ pure [J.Getstatic $ J.FieldId { J.fieldIdClass = (J.mkClassName "java/lang/System/out"), J.fieldIdName = "Ljava/io/PrintStream;", J.fieldIdType = J.IntType }]
    , compileExpr e
    , pure [J.Invokevirtual J.IntType J.MethodKey { J.methodKeyName = "java/io/PrintStream/println(I)V", J.methodKeyReturnType = Nothing, J.methodKeyParameterTypes = [ J.IntType ] } ]
    ]
  IAssignment _ v e -> do
    easm <- compileExpr e
    registerVar v
    idx <- getVarName v
    pure $ easm ++ [J.Istore $ intToWord16 idx]


compileInstant :: String -> ICode -> JVMCompiler String
compileInstant filename code = do
  varsCount <- asks M.size
  jvm <- join <$> mapM compileStmt (statements code)
  let funBody = unlines . fmap (("  "<>) . serializeOp) $ jvm
  pure $
    invocation filename ++
    unlines
    [ ""
    , ".method public static main([Ljava/lang/String;)V"
    , ".limit locals " ++ show (varsCount + 1)
    , ".limit stack " ++ show (estimateStackSize jvm)
    ] ++
    funBody ++
    "return\n" ++
    ".end method\n"
