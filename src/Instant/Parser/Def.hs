{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Instant.Parser.Def where

import           Control.Applicative        (liftA2)
import           Control.Monad
import           Control.Monad.Identity
import           Data.Void
import           Data.List as DL
import           Data.Bifunctor
import           qualified Text.Megaparsec as MP
import Text.Megaparsec ((<|>))
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Prelude hiding (parserLex)

import Instant.Syntax
import Instant.Parser.Types
import Instant.Parser.Utils

parserExprL4 :: Parser (ASTNode 'ExprL4)
parserExprL4 = MP.choice
  [ withASTMeta (pure ASTPlus) <*> (MP.try $ parserExprL3 <* parserOperator "+") <*> parserExprL4
  , ASTExprL3 <$> parserExprL3
  ]


parserExprL3 :: Parser (ASTNode 'ExprL3)
parserExprL3 = MP.choice
  [ MP.try $ (ASTExprL2 <$> parserExprL2) >>= infixL (ASTMinus <$ parserOperator "-") parserExprL2
  , ASTExprL2 <$> parserExprL2
  ]


parserExprL2 :: Parser (ASTNode 'ExprL2)
parserExprL2 = MP.choice
  [ MP.try $ (ASTExprL1 <$> parserExprL1) >>=
    infixL ( ASTMultiplication <$ parserOperator "*" <|>
             ASTDiv <$ parserOperator "/"
           ) parserExprL1
  , ASTExprL1 <$> parserExprL1
  ]


parserExprL1 :: Parser (ASTNode 'ExprL1)
parserExprL1 = MP.choice
  [ withASTMeta (pure ASTInt) <*> parserDecimal
  , withASTMeta (pure ASTVar) <*> parserIdentifier
  , ASTParens <$> parserParens parserExprL4
  ]

parserStmt :: Parser (ASTNode 'Stmt)
parserStmt = MP.choice
  [ withASTMeta (pure ASTAssignment) <*> (MP.try $ parserIdentifier <* parserOperator "=") <*> parserExprL4
  , withASTMeta (pure ASTExpr) <*> parserExprL4
  ]


ast :: Parser (ASTNode 'InstantProgram)
ast = (parserSkip *> (ASTNode <$> MP.sepBy parserStmt (parserOperator ";")) <* MP.eof)
