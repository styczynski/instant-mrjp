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

type Parser = ParsecT Void String Identity
parserASTMeta :: Parser ASTMeta
parserASTMeta = do
  p <- getSourcePos
  return $ ASTMeta
    (fromIntegral $ unPos $ sourceLine p)
    (fromIntegral $ unPos $ sourceColumn p)
    (sourceName p)

data ASTLevel
  = ExprL4 -- plus
  | ExprL3 -- minus
  | ExprL2 -- mult/div
  | ExprL1 -- paren/lit
  | Stmt -- statement
  | InstantProgram  -- program
  deriving (Eq, Show)

data ASTLevelT (astLevel :: ASTLevel) where
  LExprL4 :: ASTLevelT 'ExprL4
  LExprL3 :: ASTLevelT 'ExprL3
  LExprL2 :: ASTLevelT 'ExprL2
  LExprL1 :: ASTLevelT 'ExprL1
  LStmt :: ASTLevelT 'Stmt
  LInstantProgram :: ASTLevelT 'InstantProgram

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

type family InstantNodeQ q :: (ASTLevel)
type instance InstantNodeQ IExpr = 'ExprL4
type instance InstantNodeQ IStatement = 'Stmt
type instance InstantNodeQ ICode = 'InstantProgram

-- Dupa


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

----



class INode a where
  fromAST :: (InstantNode l ~ a) => ASTNode l -> a
  pretty :: a -> String
  parseAST :: Parser (ASTNode (InstantNodeQ a))

instance INode IExpr where
  parseAST = parseASTExprL4
    where
      --parseASTExprL4 :: Parser (ASTNode 'ExprL4)
      parseASTExprL4 = choice
        [ withASTMeta (pure ASTPlus) <*> (try $ (parseASTExprL3) <* parserOperator "+") <*> (parseASTExprL4)
        , ASTExprL3 <$> (parseASTExprL3)
        ]
      --parseASTExprL3 :: Parser (ASTNode 'ExprL3)
      parseASTExprL3 = choice
        [ try $ (ASTExprL2 <$> (parseASTExprL2)) >>= infixL (ASTMinus <$ parserOperator "-") (parseASTExprL2)
        , ASTExprL2 <$> (parseASTExprL2)
        ]
      --parseASTExprL2 :: Parser (ASTNode 'ExprL2)
      parseASTExprL2 = choice
        [ try $ (ASTExprL1 <$> (parseASTExprL1)) >>=
          infixL ( ASTMultiplication <$ parserOperator "*" <|>
                  ASTDiv <$ parserOperator "/"
                ) (parseASTExprL1)
        , ASTExprL1 <$> (parseASTExprL1)
        ]
     -- parseASTExprL1 :: Parser (ASTNode 'ExprL1)
      parseASTExprL1 = choice
        [ withASTMeta (pure ASTInt) <*> parserDecimal
        , withASTMeta (pure ASTVar) <*> parserIdentifier
        , ASTParens <$> parserParens (parseASTExprL4)
        ]

  fromAST (ASTPlus ann a b) = IExprPlus ann (fromAST a) (fromAST b)
  fromAST (ASTExprL3 e) = fromAST e
  fromAST (ASTMinus ann a b) = IExprMinus ann (fromAST a) (fromAST b)
  fromAST (ASTExprL2 e) = fromAST e
  fromAST (ASTMultiplication ann a b) = IExprMultiplication ann (fromAST a) (fromAST b)
  fromAST (ASTDiv ann a b) = IExprDiv ann (fromAST a) (fromAST b)
  fromAST (ASTExprL1 e) = fromAST e
  pretty = \case
    IExprInt _ i -> show i
    IExprVar _ s -> s
    IExprPlus _ a b -> "(" <> pretty a <> " + " <> pretty b <> ")"
    IExprMinus _ a b -> "(" <> pretty a <> " - " <> pretty b <> ")"
    IExprMultiplication _ a b -> "(" <> pretty a <> " * " <> pretty b <> ")"
    IExprDiv _ a b -> "(" <> pretty a <> " / " <> pretty b <> ")"

instance INode IStatement where
  parseAST = choice
    [ withASTMeta (pure ASTAssignment) <*> (try $ parserIdentifier <* parserOperator "=") <*> (parseAST :: (Parser (ASTNode (InstantNodeQ IExpr))))
    , withASTMeta (pure ASTExpr) <*> (parseAST :: (Parser (ASTNode (InstantNodeQ IExpr))))
    ]

  fromAST (ASTAssignment ann name e) = IAssignment ann name (fromAST e)
  fromAST (ASTExpr ann e) = IExpr ann (fromAST e)
  pretty = \case
    IExpr _ e -> pretty e
    IAssignment _ v e -> v <> " = " <> pretty e


instance INode ICode where
  parseAST = (parserSkip *> (ASTNode <$> sepBy (parseAST :: (Parser (ASTNode (InstantNodeQ IStatement)))) (parserOperator ";")) <* eof)

  fromAST (ASTNode stmts) = ICode (fmap fromAST stmts)
  pretty = foldMap ((<>"\n") . pretty) . statements


varMap :: ICode -> Map String Int
varMap code = M.fromList $ zip names [1..] where
  names = S.toList $ foldl inserter S.empty (statements code)

  inserter prev (IExpr _ _) = prev
  inserter prev (IAssignment _ v _) = S.insert v prev
