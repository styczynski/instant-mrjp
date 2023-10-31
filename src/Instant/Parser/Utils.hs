{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Instant.Parser.Utils where

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
import Instant.Logs
import Error.Diagnose
import Error.Diagnose.Compat.Megaparsec
import qualified Data.Text as T

import Instant.Parser.Types


parserASTMeta :: Parser ASTMeta
parserASTMeta = do
  p <- MP.getSourcePos
  return $ ASTMeta
    (fromIntegral $ MP.unPos $ MP.sourceLine p)
    (fromIntegral $ MP.unPos $ MP.sourceColumn p)
    (MP.sourceName p)


withASTMeta :: Parser (ASTMeta -> a) -> Parser a
withASTMeta p = liftA2 (flip ($)) parserASTMeta p


parserSkip :: Parser ()
parserSkip = L.space (void spaceChar) MP.empty MP.empty


parserLex :: Parser a -> Parser a
parserLex = L.lexeme parserSkip


parserIdentifier :: Parser String
parserIdentifier = parserLex $ liftA2 (:) lowerChar (MP.many alphaNumChar)


parserDecimal :: Parser Int
parserDecimal = parserLex $ L.decimal


parserOperator :: String -> Parser ()
parserOperator o =
  parserLex $ MP.try $ string o *> MP.notFollowedBy (MP.oneOf "=+-/*;")


parserParens :: Parser a -> Parser a
parserParens = MP.between (L.symbol parserSkip "(") (L.symbol parserSkip ")")


infixL :: Parser (ASTMeta -> a -> b -> a) -> Parser b -> a -> Parser a
infixL op p x = do
  f <- withASTMeta op
  y <- p
  let r = f x y
  infixL op p r <|> return r

