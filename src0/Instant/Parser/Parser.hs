{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Instant.Parser.Parser where

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
import Instant.Parser.Def (ast)
import Instant.Parser.Types
import Instant.Syntax
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (parserLex)

parseInstant :: String -> String -> InstantPipeline (Either String InstantProgram)
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
