{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Instant.Parse where

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

type Parser = MP.ParsecT Void String Identity
-- e=Void
-- s=String
-- m=Indentity
-- a=(ASTNode 'InstantProgram)
--type ParserResult = Either (MP.ParseErrorBundle String Void) (ASTNode 'InstantProgram)

instance HasHints Void msg where
  hints _ = mempty

parseInstant :: String -> String -> InstantPipeline (Either String (ASTNode 'InstantProgram))
parseInstant filename inp = do
  parserResult <- return $ (MP.runParser ast filename inp)
  errorStr <- return $ case parserResult of 
      Left err -> (concat . fmap MP.parseErrorPretty . MP.bundleErrors) err
      Right res -> ""
  case ((first (errorDiagnosticFromBundle Nothing "Parse error on input" Nothing) parserResult)) of
    Left diag -> do
        printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle (addFile diag filename inp :: Diagnostic String)
        return $ Left errorStr
    Right res -> return $ Right res

--
-- first
--   (concat . fmap MP.parseErrorPretty . MP.bundleErrors)
--   (MP.runParser ast filename inp)


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


parserInstantProgram :: Parser (ASTNode 'InstantProgram)
parserInstantProgram = ASTNode <$> MP.sepBy parserStmt (parserOperator ";")


ast :: Parser (ASTNode 'InstantProgram)
ast = (parserSkip *> parserInstantProgram <* MP.eof)
