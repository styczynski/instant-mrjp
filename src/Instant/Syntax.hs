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

type family InstantNode (t :: ASTLevel) :: *
type instance InstantNode 'ExprL4 = Expr
type instance InstantNode 'ExprL3 = Expr
type instance InstantNode 'ExprL2 = Expr
type instance InstantNode 'ExprL1 = Expr
type instance InstantNode 'Stmt = InstantStmt
type instance InstantNode 'InstantProgram  = Instant


toInstantNode :: ASTNode t -> InstantNode t
toInstantNode (ASTNode stmts) = Instant (fmap toInstantNode stmts)
toInstantNode (ASTExpr ann e) = IExpr ann (toInstantNode e)
toInstantNode (ASTAssignment ann name e) = IAssg ann name (toInstantNode e)
toInstantNode (ASTPlus ann a b) = EPlus ann (toInstantNode a) (toInstantNode b)
toInstantNode (ASTExprL3 e) = toInstantNode e
toInstantNode (ASTMinus ann a b) = EMinus ann (toInstantNode a) (toInstantNode b)
toInstantNode (ASTExprL2 e) = toInstantNode e
toInstantNode (ASTMultiplication ann a b) = EMult ann (toInstantNode a) (toInstantNode b)
toInstantNode (ASTDiv ann a b) = EDiv ann (toInstantNode a) (toInstantNode b)
toInstantNode (ASTExprL1 e) = toInstantNode e
toInstantNode (ASTInt ann i) = EInt ann i
toInstantNode (ASTVar ann n) = EVar ann n
toInstantNode (ASTParens e) = toInstantNode e


varMap :: Instant -> Map String Int
varMap code = M.fromList $ zip names [1..] where
  names = S.toList $ foldl inserter S.empty (instantCode code)

  inserter prev (IExpr _ _) = prev
  inserter prev (IAssg _ v _) = S.insert v prev
