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

data Ann = Ann { line :: Int, column :: Int, file :: String }
  deriving (Eq, Show)


at :: Ann -> String
at ann =
  file ann ++ ":" ++ show (line ann) ++ ":" ++ show (column ann)


data ASTNode (ASTLevel :: ASTLevel) where
  ASTPlus  :: Ann -> ASTNode 'ExprL3 -> ASTNode 'ExprL4 -> ASTNode 'ExprL4
  AST21    ::        ASTNode 'ExprL3            -> ASTNode 'ExprL4

  ASTMinus :: Ann -> ASTNode 'ExprL3 -> ASTNode 'ExprL2 -> ASTNode 'ExprL3
  AST32    ::        ASTNode 'ExprL2            -> ASTNode 'ExprL3

  ASTMult  :: Ann -> ASTNode 'ExprL2 -> ASTNode 'ExprL1 -> ASTNode 'ExprL2
  ASTDiv   :: Ann -> ASTNode 'ExprL2 -> ASTNode 'ExprL1 -> ASTNode 'ExprL2
  AST43    ::        ASTNode 'ExprL1            -> ASTNode 'ExprL2

  ASTInt   :: Ann -> Int                -> ASTNode 'ExprL1
  ASTVar   :: Ann -> String             -> ASTNode 'ExprL1
  ASTParen ::        ASTNode 'ExprL4            -> ASTNode 'ExprL1

  ASTExpr  :: Ann -> ASTNode 'ExprL4            -> ASTNode 'Stmt
  ASTAss   :: Ann -> String -> ASTNode 'ExprL4  -> ASTNode 'Stmt

  ASTNode      ::        [ASTNode 'Stmt]          -> ASTNode 'InstantProgram
deriving instance Eq (ASTNode t)
deriving instance Show (ASTNode t)

data Expr
  = EPlus Ann Expr Expr
  | EMinus Ann Expr Expr
  | EMult Ann Expr Expr
  | EDiv Ann Expr Expr
  | EInt Ann Int
  | EVar Ann String
  deriving (Eq, Show)

data InstantStmt
  = IExpr Ann Expr
  | IAssg Ann String Expr
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
entail (ASTAss ann name e) = IAssg ann name (entail e)
entail (ASTPlus ann a b) = EPlus ann (entail a) (entail b)
entail (AST21 e) = entail e
entail (ASTMinus ann a b) = EMinus ann (entail a) (entail b)
entail (AST32 e) = entail e
entail (ASTMult ann a b) = EMult ann (entail a) (entail b)
entail (ASTDiv ann a b) = EDiv ann (entail a) (entail b)
entail (AST43 e) = entail e
entail (ASTInt ann i) = EInt ann i
entail (ASTVar ann n) = EVar ann n
entail (ASTParen e) = entail e


varMap :: Instant -> Map String Int
varMap code = M.fromList $ zip names [1..] where
  names = S.toList $ foldl inserter S.empty (instantCode code)

  inserter prev (IExpr _ _) = prev
  inserter prev (IAssg _ v _) = S.insert v prev
