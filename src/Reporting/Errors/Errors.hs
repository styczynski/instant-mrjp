{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
module Reporting.Errors.Errors where

import Reporting.Errors.Def
import Reporting.Errors.Messages
import Reporting.Errors.Base as Err
import Reporting.Logs

import Parser.Types

import qualified Reporting.Errors.Position as P

import Control.Lens
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.List.NonEmpty as NEL

import Error.Diagnose

import Prelude hiding ((<>))
import qualified Reporting.Errors.Position as P
import Reporting.Errors.Base (SimpleError(SimpleError))

class Errorable a where
  describe :: a -> Err.SimpleError
  -- getOrigin :: a -> Maybe (Int, Int)
  -- getSourceName :: a -> String

instance Errorable Error where
  describe e = decodeError e
  -- getOrigin _ = (decodeError e) ^. _2
  -- getSourceName _ = ""

-- instance Errorable ErrorPack where
--   describe (ErrorPack pack) = let (_, err) = NEL.head pack in describe err
--   getOrigin (ErrorPack pack) = let (errorMeta, _) = NEL.head pack in errorMeta

numberedLineStr :: (String -> String) -> (Int, String) -> String
numberedLineStr fn (no, line) = "  " ++ (show no) ++ ".) " ++ (fn line)

lineMap :: [String] -> (String -> String) -> String
lineMap l fn = let numberedLines = zip [1..(L.length l)] l in L.intercalate "\n" (L.map (numberedLineStr fn) numberedLines)

printErrors :: (Errorable err) => err -> String -> String -> RawProgram -> LattePipeline ()
printErrors errorable filename inp ast = do
  simpleError <- return $ describe errorable
  title <- return $ "Problem: " ++ (simpleError^.Err.name)
  description <- return $ simpleError^.description
  origin <- return $ simpleError^.Err.location
  suggestions <- return $ case (simpleError^.Err.sugestions) of
    [] -> []
    lines -> [Note $ "This that can potentially help:\n" ++ lineMap lines id]
  diagnostic <- return $ addReport (addFile mempty filename inp :: Diagnostic String) (Err Nothing title (getMarkers simpleError filename origin) ([] ++ suggestions))
  printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle diagnostic
  where
    --getMarkers :: String -> (Maybe Position) -> ([])
    getMarkers :: SimpleError -> String -> (Maybe P.Position) -> [(Position, Marker String)]
    getMarkers simpleError filename Nothing =
      let errorDescription = simpleError^.description
          markers = [(Position (0, 0) (0, 1) filename, This errorDescription)]
          contextMarkers = L.map (mapContext filename) $ simpleError^.Err.contexts
          helpMarkers = case (simpleError^.Err.help) of
              Nothing -> []
              (Just (label, p)) -> [(Position (P.positionLC p) (P.positionLC $ findTokenEnd ast p) (P.positionSrc p), Maybe $ "Action: " ++ label)]
      in markers ++ contextMarkers ++ helpMarkers
    getMarkers simpleError _ (Just errLocation) =
      let errorDescription = simpleError^.description
          markers = [(Position (P.positionLC errLocation) (P.positionLC $ findTokenEnd ast errLocation) (P.positionSrc errLocation), This errorDescription)]
          helpMarkers = case (simpleError^.Err.help) of
              Nothing -> []
              (Just (label, p)) -> [(Position (P.positionLC p) (P.positionLC $ findTokenEnd ast p) (P.positionSrc p), Maybe $ "Action: " ++ label)]
          contextMarkers = L.map (mapContext $ P.positionSrc errLocation) $ simpleError^.Err.contexts
      in markers ++ contextMarkers ++ helpMarkers

    mapContext :: String -> (String, Maybe P.Position) -> (Position, Marker String)
    mapContext origFilename (label, labelLoc) = case labelLoc of
      Nothing -> (Position (1, 1) (1, 1) origFilename, Where label)
      Just p -> (Position (P.positionLC p) (P.positionLC $ findTokenEnd ast p) (P.positionSrc p), Where label)

  