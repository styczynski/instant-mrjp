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
import           Prelude hiding (lex)

import Instant.Syntax


type Parser = ParsecT Void String Identity
parseInstant :: String -> String -> Either String (ASTNode 'InstantProgram)
parseInstant filename inp = first
  (concat . fmap parseErrorPretty . bundleErrors)
  (parse ast filename inp)


getASTMeta :: Parser ASTMeta
getASTMeta = do
  p <- getSourcePos
  return $ ASTMeta
    (fromIntegral $ unPos $ sourceLine p)
    (fromIntegral $ unPos $ sourceColumn p)
    (sourceName p)


withASTMeta :: Parser (ASTMeta -> a) -> Parser a
withASTMeta p = liftA2 (flip ($)) getASTMeta p


skip :: Parser ()
skip = L.space (void spaceChar) empty empty


lex :: Parser a -> Parser a
lex = L.lexeme skip


lId :: Parser String
lId = lex $ liftA2 (:) lowerChar (many alphaNumChar)


unsigned :: Parser Int
unsigned = lex $ L.decimal


operator :: String -> Parser ()
operator o =
  lex $ try $ string o *> notFollowedBy (oneOf "=+-/*;")


paren :: Parser a -> Parser a
paren = between (L.symbol skip "(") (L.symbol skip ")")


infixL :: Parser (ASTMeta -> a -> b -> a) -> Parser b -> a -> Parser a
infixL op p x = do
  f <- withASTMeta op
  y <- p
  let r = f x y
  infixL op p r <|> return r


astE1 :: Parser (ASTNode 'ExprL4)
astE1 = choice
  [ withASTMeta (pure ASTPlus) <*> (try $ astE2 <* operator "+") <*> astE1
  , ASTExprL3 <$> astE2
  ]


astE2 :: Parser (ASTNode 'ExprL3)
astE2 = choice
  [ try $ (ASTExprL2 <$> astE3) >>= infixL (ASTMinus <$ operator "-") astE3
  , ASTExprL2 <$> astE3
  ]


astE3 :: Parser (ASTNode 'ExprL2)
astE3 = choice
  [ try $ (ASTExprL1 <$> astE4) >>=
    infixL ( ASTMultiplication <$ operator "*" <|>
             ASTDiv <$ operator "/"
           ) astE4
  , ASTExprL1 <$> astE4
  ]


astE4 :: Parser (ASTNode 'ExprL1)
astE4 = choice
  [ withASTMeta (pure ASTInt) <*> unsigned
  , withASTMeta (pure ASTVar) <*> lId
  , ASTParens <$> paren astE1
  ]

astSt :: Parser (ASTNode 'Stmt)
astSt = choice
  [ withASTMeta (pure ASTAssignment) <*> (try $ lId <* operator "=") <*> astE1
  , withASTMeta (pure ASTExpr) <*> astE1
  ]


astP :: Parser (ASTNode 'InstantProgram)
astP = ASTNode <$> sepBy astSt (operator ";")


ast :: Parser (ASTNode 'InstantProgram)
ast = (skip *> astP <* eof)
