module Instant.Interpret where

import qualified Data.Map as M
import           Data.Map(Map)
import           Control.Applicative
import           Control.Monad.RWS.Strict
import           Control.Monad.Except

import Instant.Syntax


type Interpret = RWST () String (Map String Int) (Except String)


eval :: ICode -> Interpret ()
eval code = void $ traverse evalStmt (statements code)


evalStmt :: IStatement -> Interpret ()
evalStmt = \case
  IExpr _ e -> evalExpr e >>= \i -> tell (show i <> "\n")
  IAssignment _ n e -> evalExpr e >>= \i -> modify (M.insert n i)


evalExpr :: IExpr -> Interpret Int
evalExpr = \case
  IExprInt _ i -> pure i
  IExprVar ann v -> gets (M.lookup v) >>=
    maybe (throwError $ at ann ++ " No such variable " ++ v) pure
  IExprPlus _ a b  -> liftA2 (+) (evalExpr a) (evalExpr b)
  IExprMinus _ a b -> liftA2 (-) (evalExpr a) (evalExpr b)
  IExprMultiplication _ a b  -> liftA2 (*) (evalExpr a) (evalExpr b)
  IExprDiv _ a b   -> liftA2 div (evalExpr a) (evalExpr b)


interpret :: ICode -> Either String String
interpret inst =
  runExcept (fmap (\(_, _, res) -> res) (runRWST (eval inst) () M.empty))
