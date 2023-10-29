module Instant.PP where

import Instant.Syntax


ppExpr :: Expr -> String
ppExpr = \case
  IExprInt _ i -> show i
  IExprVar _ s -> s
  IExprPlus _ a b -> "(" <> ppExpr a <> " + " <> ppExpr b <> ")"
  IExprMinus _ a b -> "(" <> ppExpr a <> " - " <> ppExpr b <> ")"
  IExprMultiplication _ a b -> "(" <> ppExpr a <> " * " <> ppExpr b <> ")"
  IExprDiv _ a b -> "(" <> ppExpr a <> " / " <> ppExpr b <> ")"


ppStmt :: IStatement -> String
ppStmt = \case
  IExpr _ e -> ppExpr e
  IAssignment _ v e -> v <> " = " <> ppExpr e


ppInstant :: ICode -> String
ppInstant = foldMap ((<>"\n") . ppStmt) . statements
