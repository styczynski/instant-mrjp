{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Instant.Syntax where

import qualified Data.Set as S
import qualified Data.Map as M
import           Data.Map(Map)

import           Control.Applicative        (liftA2)
import           Control.Monad
import           Control.Monad.Identity
import           Data.Void
import           Data.List as DL
import           Data.Bifunctor
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Prelude hiding (parserLex)

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

data IExpr
  = IExprPlus ASTMeta IExpr IExpr
  | IExprMinus ASTMeta IExpr IExpr
  | IExprMultiplication ASTMeta IExpr IExpr
  | IExprDiv ASTMeta IExpr IExpr
  | IExprInt ASTMeta Int
  | IExprVar ASTMeta String
  deriving (Eq, Show)

data IStatement
  = IExpr ASTMeta IExpr
  | IAssignment ASTMeta String IExpr
  deriving (Eq, Show)

newtype ICode = ICode { statements :: [IStatement] }
  deriving (Eq, Show)

type family InstantNode (t :: ASTLevel) :: *
type instance InstantNode 'ExprL4 = IExpr
type instance InstantNode 'ExprL3 = IExpr
type instance InstantNode 'ExprL2 = IExpr
type instance InstantNode 'ExprL1 = IExpr
type instance InstantNode 'Stmt = IStatement
type instance InstantNode 'InstantProgram  = ICode


class INode a where
  fromAST :: (InstantNode l ~ a) => ASTNode l -> a
  pretty :: a -> String

instance INode IExpr where
  fromAST (ASTPlus ann a b) = IExprPlus ann (fromAST a) (fromAST b)
  fromAST (ASTExprL3 e) = fromAST e
  fromAST (ASTMinus ann a b) = IExprMinus ann (fromAST a) (fromAST b)
  fromAST (ASTExprL2 e) = fromAST e
  fromAST (ASTMultiplication ann a b) = IExprMultiplication ann (fromAST a) (fromAST b)
  fromAST (ASTDiv ann a b) = IExprDiv ann (fromAST a) (fromAST b)
  fromAST (ASTExprL1 e) = fromAST e
  fromAST (ASTInt ann i) = IExprInt ann i
  fromAST (ASTVar ann n) = IExprVar ann n
  fromAST (ASTParens e) = fromAST e
  pretty = \case
    IExprInt _ i -> show i
    IExprVar _ s -> s
    IExprPlus _ a b -> "(" <> pretty a <> " + " <> pretty b <> ")"
    IExprMinus _ a b -> "(" <> pretty a <> " - " <> pretty b <> ")"
    IExprMultiplication _ a b -> "(" <> pretty a <> " * " <> pretty b <> ")"
    IExprDiv _ a b -> "(" <> pretty a <> " / " <> pretty b <> ")"

instance INode IStatement where
  fromAST (ASTAssignment ann name e) = IAssignment ann name (fromAST e)
  fromAST (ASTExpr ann e) = IExpr ann (fromAST e)
  pretty = \case
    IExpr _ e -> pretty e
    IAssignment _ v e -> v <> " = " <> pretty e


instance INode ICode where
  fromAST (ASTNode stmts) = ICode (fmap fromAST stmts)
  pretty = foldMap ((<>"\n") . pretty) . statements


varMap :: ICode -> Map String Int
varMap code = M.fromList $ zip names [1..] where
  names = S.toList $ foldl inserter S.empty (statements code)

  inserter prev (IExpr _ _) = prev
  inserter prev (IAssignment _ v _) = S.insert v prev
