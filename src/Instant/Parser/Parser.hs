{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Instant.Parser.Parser where

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
import Instant.Parser.Def(ast)


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

