module Instant.Parse where

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

import Instant.Syntax


type Parser = ParsecT Void String Identity
parseInstant :: String -> String -> Either String (ASTNode 'InstantProgram)
parseInstant filename inp = first
  (concat . fmap parseErrorPretty . bundleErrors)
  (parse ast filename inp)


parserASTMeta :: Parser ASTMeta
parserASTMeta = do
  p <- getSourcePos
  return $ ASTMeta
    (fromIntegral $ unPos $ sourceLine p)
    (fromIntegral $ unPos $ sourceColumn p)
    (sourceName p)


withASTMeta :: Parser (ASTMeta -> a) -> Parser a
withASTMeta p = liftA2 (flip ($)) parserASTMeta p


parserSkip :: Parser ()
parserSkip = L.space (void spaceChar) empty empty


parserLex :: Parser a -> Parser a
parserLex = L.lexeme parserSkip


parserIdentifier :: Parser String
parserIdentifier = parserLex $ liftA2 (:) lowerChar (many alphaNumChar)


parserDecimal :: Parser Int
parserDecimal = parserLex $ L.decimal


parserOperator :: String -> Parser ()
parserOperator o =
  parserLex $ try $ string o *> notFollowedBy (oneOf "=+-/*;")


parserParens :: Parser a -> Parser a
parserParens = between (L.symbol parserSkip "(") (L.symbol parserSkip ")")


infixL :: Parser (ASTMeta -> a -> b -> a) -> Parser b -> a -> Parser a
infixL op p x = do
  f <- withASTMeta op
  y <- p
  let r = f x y
  infixL op p r <|> return r


parserExprL4 :: Parser (ASTNode 'ExprL4)
parserExprL4 = choice
  [ withASTMeta (pure ASTPlus) <*> (try $ parserExprL3 <* parserOperator "+") <*> parserExprL4
  , ASTExprL3 <$> parserExprL3
  ]


parserExprL3 :: Parser (ASTNode 'ExprL3)
parserExprL3 = choice
  [ try $ (ASTExprL2 <$> parserExprL2) >>= infixL (ASTMinus <$ parserOperator "-") parserExprL2
  , ASTExprL2 <$> parserExprL2
  ]


parserExprL2 :: Parser (ASTNode 'ExprL2)
parserExprL2 = choice
  [ try $ (ASTExprL1 <$> parserExprL1) >>=
    infixL ( ASTMultiplication <$ parserOperator "*" <|>
             ASTDiv <$ parserOperator "/"
           ) parserExprL1
  , ASTExprL1 <$> parserExprL1
  ]


parserExprL1 :: Parser (ASTNode 'ExprL1)
parserExprL1 = choice
  [ withASTMeta (pure ASTInt) <*> parserDecimal
  , withASTMeta (pure ASTVar) <*> parserIdentifier
  , ASTParens <$> parserParens parserExprL4
  ]

parserStmt :: Parser (ASTNode 'Stmt)
parserStmt = choice
  [ withASTMeta (pure ASTAssignment) <*> (try $ parserIdentifier <* parserOperator "=") <*> parserExprL4
  , withASTMeta (pure ASTExpr) <*> parserExprL4
  ]


parserInstantProgram :: Parser (ASTNode 'InstantProgram)
parserInstantProgram = ASTNode <$> sepBy parserStmt (parserOperator ";")


ast :: Parser (ASTNode 'InstantProgram)
ast = (parserSkip *> parserInstantProgram <* eof)
