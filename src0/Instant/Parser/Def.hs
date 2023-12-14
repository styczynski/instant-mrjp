{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Instant.Parser.Def where

import Control.Applicative (liftA2)
import Control.Monad
import Control.Monad.Identity
import Data.Bifunctor
import Data.List as DL
import qualified Data.List.NonEmpty as NEL
import           Control.Applicative.Combinators.NonEmpty as NEC
import Data.Void
import Instant.Parser.Types
import Instant.Parser.Utils
import Instant.Syntax
import Text.Megaparsec ((<|>))
import           Data.Functor(void, ($>))
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (parserLex)

import qualified Instant.Syntax as AST

keywords :: [String]
keywords =
  [ "return"
  , "while", "if" , "else"
  , "int", "string", "boolean", "void"
  , "true", "false"
  , "class", "public", "private", "protected"
  , "static", "new"
  , "super"
  ]

validateIdentifier :: Parser String -> Parser String
validateIdentifier ps = MP.try $ do
  s <- ps
  when (Prelude.take 2 s == "__" || s `elem` keywords) $
    fail ("invalid id: " ++ s)
  return s

-- TODO: Rename

varId :: Parser AST.VarId
varId = AST.VarId <$> validateIdentifier parserIdentifier

funId :: Parser AST.FunId
funId = AST.FunId <$> validateIdentifier parserIdentifier

classId :: Parser AST.ClassId
classId = AST.ClassId <$> validateIdentifier parserCapIdentifier

fieldId :: Parser AST.FieldId
fieldId =  AST.FieldId <$> validateIdentifier parserIdentifier

methodId :: Parser AST.MethodId
methodId =  AST.MethodId <$> validateIdentifier parserIdentifier

constructorId :: Parser AST.ConstructorId
constructorId = AST.ConstructorId <$> validateIdentifier parserIdentifier


lit :: Parser AST.Lit
lit = MP.choice
  [ AST.LInt <$> parserDecimal
  , AST.LString <$> (char '\"' *> MP.manyTill L.charLiteral (char '\"'))
  , AST.LBool <$> ((True <$ parserKeyword "true") <|> (False <$ parserKeyword "false"))
  ]


relOp :: Parser (AST.Op 'AST.Rel)
relOp = MP.choice
  [ withASTMeta $ AST.LT  <$ parserOperator "<"
  , withASTMeta $ AST.LEQ <$ parserOperator "<="
  , withASTMeta $ AST.EQ  <$ parserOperator "=="
  , withASTMeta $ AST.GEQ <$ parserOperator ">="
  , withASTMeta $ AST.GT  <$ parserOperator ">"
  , withASTMeta $ AST.NEQ <$ parserOperator "!="
  ]

addOp :: Parser (AST.Op 'AST.Add)
addOp = MP.choice
  [ withASTMeta $ AST.Plus  <$ parserOperator "+"
  , withASTMeta $ AST.Minus <$ parserOperator "-"
  ]

mulOp :: Parser (AST.Op 'AST.Mul)
mulOp = MP.choice
  [ withASTMeta $ AST.Mult <$ parserOperator "*"
  , withASTMeta $ AST.Div  <$ parserOperator "/"
  , withASTMeta $ AST.Mod  <$ parserOperator "%"
  ]

expr :: Parser (AST.Expr 'AST.Untyped)
expr = AST.fromAST <$> expr0

rawExpr :: Parser (AST.ASTNodeChild 0)
rawExpr = expr0

expr0 :: Parser (AST.ASTNodeChild 0)
expr0 = do
  e <- expr1
  MP.choice [ withRawASTMeta AST.REOr <*> (parserOperator "||" $> e) <*> expr0
         , pure $ AST.RECoe e
         ]


expr1 :: Parser (AST.ASTNodeChild 1)
expr1 = do
  e <- expr2
  MP.choice [ withRawASTMeta AST.REAnd <*> (parserOperator "&&" $> e) <*> expr1
         , pure $ AST.RECoe e
         ]



expr2 :: Parser (AST.ASTNodeChild 2)
expr2 = do
  e <- AST.RECoe <$> expr3
  MP.choice [ MP.try $ (pure e) >>= infixL (withASTMeta $ flip AST.RERelOp <$> relOp) expr3
         , pure e
         ]


expr3 :: Parser (AST.ASTNodeChild 3)
expr3 = do
  e <- AST.RECoe <$> expr4
  MP.choice [ MP.try $ (pure e) >>= infixL (withASTMeta $ flip AST.REAddOp <$> addOp) expr4
         , pure e
         ]


expr4 :: Parser (AST.ASTNodeChild 4)
expr4 = do
  e <- AST.RECoe <$> expr5
  MP.choice [ MP.try $ (pure e) >>= infixL (withASTMeta $ flip AST.REMulOp <$> mulOp) expr5
         , pure e
         ]


expr5 :: Parser (AST.ASTNodeChild 5)
expr5 = MP.choice
  [ 
  parserOperator "!" *> withRawASTMeta AST.RENot <*> expr6
  , parserOperator "-" *> withRawASTMeta AST.RENeg <*> expr6
  , AST.RECoe <$> expr6
  ]


expr6 :: Parser (AST.ASTNodeChild 6)
expr6 = do
  e <- AST.RECoe <$> expr7
  let proj = MP.choice
        [ Right <$> MP.try (liftA2 (,) methodId appliedArgs)
        , Left <$> fieldId
        ]
      fldOrMth a l r = case r of
        Left i -> AST.REProj a l i
        Right (i, as) -> AST.REMApp a l i as
  MP.choice [ MP.try $ (pure e) >>= infixL (withASTMeta $ parserOperator "." $> fldOrMth) proj
         , pure e
         ]


expr7 :: Parser (AST.ASTNodeChild 7)
expr7 = MP.choice
  [ withRawASTMeta AST.REPar <*> parserParens expr0
  , withASTMeta (AST.RESuper <$ parserKeyword "super")
  , withRawASTMeta AST.RELit <*> lit
  , MP.try $ withRawASTMeta AST.REApp <*> funId <*> appliedArgs
  , withRawASTMeta AST.RENew <*> (parserKeyword "new" *> classId) <*> MP.optional (parserSymbol "." *> constructorId)
    <*> parserParens (MP.sepBy expr0 (parserSymbol ","))
  , withRawASTMeta AST.REVar <*> varId
  ]


rawDecl :: AST.IsId id => Parser id -> Parser (id, Maybe (AST.ASTNodeChild 0))
rawDecl p = liftA2 (,) (MP.try $ p <* parserOperator "=") (Just <$> rawExpr)
  <|> liftA2 (,) p (pure Nothing)

rawDecls :: AST.IsId id => Parser id -> Parser (NEL.NonEmpty (id, Maybe (AST.ASTNodeChild 0)))
rawDecls p = NEC.sepBy1 (rawDecl p) (parserSymbol ",")

decl :: AST.IsId id => Parser id -> Parser (id, Maybe (AST.Expr 'AST.Untyped))
decl p = fmap (fmap AST.fromAST) <$> rawDecl p

decls :: AST.IsId id => Parser id -> Parser (NEL.NonEmpty (id, Maybe (AST.Expr 'AST.Untyped)))
decls p = NEC.sepBy1 (decl p) (parserSymbol ",")


semicolon :: Parser ()
semicolon = void $ parserSymbol ";"


appliedArgs :: Parser [AST.ASTNodeChild 0]
appliedArgs = parserParens (MP.sepBy expr0 (parserSymbol ","))


stmt :: Parser AST.ASTNode
stmt = MP.choice
  [ parseBlock
  , withRawASTMeta AST.RSAssg <*> MP.try (varId <* parserOperator "=") <*> rawExpr <* semicolon
  , MP.try $ withRawASTMeta AST.RSDecl <*> type_ <*> rawDecls varId <* semicolon
  , withRawASTMeta AST.RSIncr <*> MP.try (varId <* parserOperator "++") <* semicolon
  , withRawASTMeta AST.RSDecr <*> MP.try (varId <* parserOperator "--") <* semicolon
  , withRawASTMeta AST.RSRet <*> (MP.try $ parserKeyword "return" *> rawExpr) <* semicolon
  , withASTMeta (AST.RSVRet <$ parserKeyword "return") <* semicolon
  , withRawASTMeta (\a c t me -> case me of
                 Nothing -> AST.RSCond a c t
                 Just e -> AST.RSCondElse a c t e
             )
    <*> (parserKeyword "if" *> parserParens rawExpr) <*> stmt <*> MP.optional (parserKeyword "else" *> stmt)
  , withRawASTMeta AST.RSWhile <*> (parserKeyword "while" *> parserParens rawExpr) <*> stmt
  , withRawASTMeta (\a e mv -> case (mv, e) of
                 (Just v, AST.RECoe
                   (AST.RECoe (AST.RECoe (AST.RECoe (AST.RECoe (AST.RECoe (AST.REProj _ea ee ei))))))) ->
                   AST.RSFieldAssg a (cccoe ee) ei v where
                   cccoe = AST.RECoe . AST.RECoe . AST.RECoe . AST.RECoe . AST.RECoe . AST.RECoe
                 _ -> AST.RSExp a e
             ) <*> rawExpr <*> MP.optional (parserOperator "=" *> rawExpr) <* semicolon
  , withASTMeta (pure AST.RSEmpty) <* semicolon
  ]


parseBlock :: Parser AST.ASTNode
parseBlock = withRawASTMeta AST.RSBlock <*> parserBlock (MP.many stmt)


parseBody :: Parser (AST.Stmt 'AST.Untyped)
parseBody = AST.fromAST <$> parseBlock


type_ :: Parser AST.Type
type_ = MP.choice
  [ AST.TInt <$ parserKeyword "int"
  , AST.TBool <$ parserKeyword "boolean"
  , AST.TString <$ parserKeyword "string"
  , AST.TVoid <$ parserKeyword "void"
  , AST.TClass <$> classId
  ]


arg :: Parser AST.Arg
arg = withRawASTMeta AST.Arg <*> type_ <*> varId

parseArgs :: Parser [AST.Arg]
parseArgs = parserParens (MP.sepBy arg (parserSymbol ","))


topDef :: Parser (AST.TopDef 'AST.Untyped)
topDef = MP.choice
  [ AST.TDFun <$> funDef
  , AST.TDClass <$> classDef
  ]


classDef :: Parser (AST.ClassDef 'AST.Untyped)
classDef = parserKeyword "class" *> withRawASTMeta AST.ClassDef
  <*> classId
  <*> MP.optional (parserKeyword "extends" *> classId)
  <*> parserBlock (MP.many classMember)


funDef :: Parser (AST.FunDef 'AST.Untyped)
funDef = withRawASTMeta AST.FunDef
  <*> type_
  <*> funId
  <*> parseArgs
  <*> parseBody


classMember :: Parser (AST.ClassMember 'AST.Untyped)
classMember = MP.choice
  [ AST.CMField <$> MP.try field
  , AST.CMMethod <$> method
  , AST.CMConstructor <$> constructor
  ]


method :: Parser (AST.Method 'AST.Untyped)
method = withRawASTMeta AST.Method
  <*> classMemberAccess AST.Protected
  <*> classMemberPlace
  <*> type_
  <*> methodId
  <*> parseArgs
  <*> MP.optional parseBody


field :: Parser (AST.Field 'AST.Untyped)
field = withRawASTMeta AST.Field
  <*> classMemberAccess AST.Protected
  <*> classMemberPlace
  <*> type_
  <*> decls fieldId
  <* semicolon


constructor :: Parser (AST.Constructor 'AST.Untyped)
constructor = parserKeyword "new" *> withRawASTMeta AST.Constructor
  <*> classMemberAccess AST.Public
  <*> MP.optional constructorId
  <*> parseArgs
  <*> parseBody


classMemberAccess :: AST.ClassMemberAccess -> Parser AST.ClassMemberAccess
classMemberAccess dflt =
  parserKeyword "private" $> AST.Private <|>
  parserKeyword "protected" $> AST.Protected <|>
  parserKeyword "public" $> AST.Public <|>
  pure dflt


classMemberPlace :: Parser AST.ClassMemberPlace
classMemberPlace =
  parserKeyword "static" $> AST.Static <|> pure AST.Dynamic

parseProgram :: Parser [AST.TopDef 'AST.Untyped]
parseProgram = MP.many topDef

ast :: Parser AST.InstantProgram
ast = do
  stmts <- parseProgram
  return AST.ICode { statements = stmts }