{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
module Instant.Errors.Errors where

import Instant.Syntax
import Instant.Errors.Def
import Instant.Errors.Messages
import Control.Lens
import Instant.Logs
import qualified Data.Text as T
import qualified Data.List as L
import Instant.Errors.Base as Err
import qualified Data.List.NonEmpty as NEL
import qualified Instant.Syntax as AST

import Error.Diagnose
import Error.Diagnose.Compat.Megaparsec

import Prelude hiding ((<>))

class Errorable a where
  describe :: a -> Err.SimpleError
  errorMeta :: a -> Maybe AST.ASTMeta
  getMeta :: a -> AST.ASTMeta
  default getMeta :: a -> AST.ASTMeta
  getMeta err = case errorMeta err of  
    Nothing -> AST.emptyMeta
    Just meta -> meta

newtype ErrorPack = ErrorPack (NEL.NonEmpty (Maybe AST.ASTMeta, Error))
  deriving (Semigroup)

instance Errorable Error where
  describe e = decodeError e
  errorMeta _ = Just AST.emptyMeta

instance Errorable ErrorPack where
  describe (ErrorPack pack) = let (_, err) = NEL.head pack in describe err
  errorMeta (ErrorPack pack) = let (errorMeta, _) = NEL.head pack in errorMeta

numberedLineStr :: (String -> String) -> (Int, String) -> String
numberedLineStr fn (no, line) = "  " ++ (show no) ++ ".) " ++ (fn line)

lineMap :: [String] -> (String -> String) -> String
lineMap l fn = let numberedLines = zip [1..(L.length l)] l in L.intercalate "\n" (L.map (numberedLineStr fn) numberedLines)

printErrors :: (Errorable err) => err -> String -> String -> InstantPipeline ()
printErrors errorable filename inp = do
  simpleError <- return $ describe errorable
  title <- return $ "Problem: " ++ (simpleError^.Err.name)
  description <- return $ simpleError^.description
  (line, col) <- return ((AST.line $ getMeta errorable), (AST.column $ getMeta errorable))
  markers <- return $ [(Position (line, 1) (line, col) (AST.file $ getMeta errorable), This description)]
  sourceClassMarkers <- return $ case simpleError^.Err.oocontext of
    Nothing -> []
    Just clsName -> [(Position (line, 1) (line, col) (AST.file $ getMeta errorable), Where clsName)]
  suggestions <- return $ case (simpleError^.Err.sugestions) of
    [] -> []
    lines -> [Note $ "This that can potentially help:\n" ++ lineMap lines id]
  diagnostic <- return $ addReport (addFile mempty filename inp :: Diagnostic String) (Err Nothing title (markers ++ sourceClassMarkers) ([] ++ suggestions))
  printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle diagnostic