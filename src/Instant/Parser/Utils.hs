{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Instant.Parser.Utils where

import Control.Applicative (liftA2)
import Control.Monad
import Control.Monad.Identity
import Data.Bifunctor
import Data.List as DL
import qualified Data.Text as T
import Data.Void
import Error.Diagnose
import Error.Diagnose.Compat.Megaparsec
import Instant.Logs
import Instant.Parser.Types
import Instant.Syntax
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (parserLex)

parserASTMeta :: Parser ASTMeta
parserASTMeta = do
  p <- MP.getSourcePos
  return $
    ASTMeta
      (fromIntegral $ MP.unPos $ MP.sourceLine p)
      (fromIntegral $ MP.unPos $ MP.sourceColumn p)
      (MP.sourceName p)

withASTMeta :: Parser (ASTMeta -> a) -> Parser a
withASTMeta p = liftA2 (flip ($)) parserASTMeta p

withRawASTMeta :: (ASTMeta -> a) -> Parser a
withRawASTMeta = withASTMeta . pure

parserSkip :: Parser ()
parserSkip = L.space
  (void spaceChar)
  (L.skipLineComment "//" <|> L.skipLineComment "#")
  (L.skipBlockComment "/*" "*/")

parserLex :: Parser a -> Parser a
parserLex = L.lexeme parserSkip

parserIdentifier :: Parser String
parserIdentifier = parserLex $ liftA2 (:) lowerChar (MP.many alphaNumChar)

parserCapIdentifier :: Parser String
parserCapIdentifier = parserLex $ liftA2 (:) upperChar (MP.many alphaNumChar)

parserDecimal :: Parser Integer
parserDecimal = parserLex $ L.decimal

parserSymbol :: String -> Parser String
parserSymbol = L.symbol parserSkip

parserKeyword :: String -> Parser ()
parserKeyword w = parserLex $ MP.try $ string w >> MP.notFollowedBy alphaNumChar >> parserSkip

parserOperator :: String -> Parser ()
parserOperator o =
  parserLex $ MP.try $ string o *> MP.notFollowedBy (MP.oneOf ("=+-/*%\\&.|^<>" :: String))

parserParens :: Parser a -> Parser a
parserParens = MP.between (L.symbol parserSkip "(") (L.symbol parserSkip ")")

parserBlock :: Parser a -> Parser a
parserBlock = MP.between (L.symbol parserSkip "{") (L.symbol parserSkip "}")

infixL :: Parser (a -> b -> a) -> Parser b -> a -> Parser a
infixL op p x = do
  f <- op
  y <- p
  let r = f x y
  infixL op p r <|> return r