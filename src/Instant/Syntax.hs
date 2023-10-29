{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Instant.Syntax where

import qualified Data.Set as S
import qualified Data.Map as M
import           Data.Map(Map)


data ASTLevel
  = ExprL4 -- plus
  | ExprL3 -- minus
  | ExprL2 -- mult/div
  | ExprL1 -- paren/lit
  | Stmt -- statement
  | InstantProgram  -- program
  deriving (Eq, Show)

data ASTMeta = ASTMeta  { line :: Int, column :: Int, file :: String }
  deriving (Eq, Show)


at :: ASTMeta -> String
at astMeta =
  file astMeta ++ ":" ++ show (line astMeta) ++ ":" ++ show (column astMeta)


data ASTNode (astLevel :: ASTLevel) where
  ASTPlus  :: ASTMeta -> ASTNode 'ExprL3 -> ASTNode 'ExprL4 -> ASTNode 'ExprL4
  ASTExprL3    ::        ASTNode 'ExprL3            -> ASTNode 'ExprL4

  ASTMinus :: ASTMeta -> ASTNode 'ExprL3 -> ASTNode 'ExprL2 -> ASTNode 'ExprL3
  ASTExprL2    ::        ASTNode 'ExprL2            -> ASTNode 'ExprL3

  ASTMultiplication  :: ASTMeta -> ASTNode 'ExprL2 -> ASTNode 'ExprL1 -> ASTNode 'ExprL2
  ASTDiv   :: ASTMeta -> ASTNode 'ExprL2 -> ASTNode 'ExprL1 -> ASTNode 'ExprL2
  ASTExprL1    ::        ASTNode 'ExprL1            -> ASTNode 'ExprL2

  ASTInt   :: ASTMeta -> Int                -> ASTNode 'ExprL1
  ASTVar   :: ASTMeta -> String             -> ASTNode 'ExprL1
  ASTParens ::        ASTNode 'ExprL4            -> ASTNode 'ExprL1

  ASTExpr  :: ASTMeta -> ASTNode 'ExprL4            -> ASTNode 'Stmt
  ASTAssignment   :: ASTMeta -> String -> ASTNode 'ExprL4  -> ASTNode 'Stmt

  ASTNode      ::        [ASTNode 'Stmt]          -> ASTNode 'InstantProgram
deriving instance Eq (ASTNode t)
deriving instance Show (ASTNode t)

data Expr
  = EPlus ASTMeta Expr Expr
  | EMinus ASTMeta Expr Expr
  | EMult ASTMeta Expr Expr
  | EDiv ASTMeta Expr Expr
  | EInt ASTMeta Int
  | EVar ASTMeta String
  deriving (Eq, Show)

data InstantStmt
  = IExpr ASTMeta Expr
  | IAssg ASTMeta String Expr
  deriving (Eq, Show)

newtype Instant = Instant { instantCode :: [InstantStmt] }
  deriving (Eq, Show)

type family EntailedBy (t :: ASTLevel) :: *
type instance EntailedBy 'ExprL4 = Expr
type instance EntailedBy 'ExprL3 = Expr
type instance EntailedBy 'ExprL2 = Expr
type instance EntailedBy 'ExprL1 = Expr
type instance EntailedBy 'Stmt = InstantStmt
type instance EntailedBy 'InstantProgram  = Instant


entail :: ASTNode t -> EntailedBy t
entail (ASTNode stmts) = Instant (fmap entail stmts)
entail (ASTExpr ann e) = IExpr ann (entail e)
entail (ASTAssignment ann name e) = IAssg ann name (entail e)
entail (ASTPlus ann a b) = EPlus ann (entail a) (entail b)
entail (ASTExprL3 e) = entail e
entail (ASTMinus ann a b) = EMinus ann (entail a) (entail b)
entail (ASTExprL2 e) = entail e
entail (ASTMultiplication ann a b) = EMult ann (entail a) (entail b)
entail (ASTDiv ann a b) = EDiv ann (entail a) (entail b)
entail (ASTExprL1 e) = entail e
entail (ASTInt ann i) = EInt ann i
entail (ASTVar ann n) = EVar ann n
entail (ASTParens e) = entail e


varMap :: Instant -> Map String Int
varMap code = M.fromList $ zip names [1..] where
  names = S.toList $ foldl inserter S.empty (instantCode code)

  inserter prev (IExpr _ _) = prev
  inserter prev (IAssg _ v _) = S.insert v prev
