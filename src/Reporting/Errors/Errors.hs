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
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Reporting.Errors.Position as P

import Control.Lens
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.List.NonEmpty as NEL

import Error.Diagnose
import Data.Maybe
import Prettyprinter.Render.Text
import Prettyprinter (Doc, Pretty, hardline, pretty, defaultLayoutOptions, reAnnotateS, layoutPretty)

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
  case (simpleError^.Err.markers) of
    Nothing -> return ()
    (Just markers) -> printDebugContextMarkers filename inp ast markers
  where
    printDebugContextMarkers :: String -> String -> RawProgram -> DebugContextMarker -> LattePipeline ()
    printDebugContextMarkers filename inp ast marker = liftIO $ putStrLn (formatDebugContextMarker filename inp ast marker)
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

formatDebugContextMarker :: String -> String -> RawProgram -> DebugContextMarker -> String
formatDebugContextMarker filename inp ast m@(MarkNothing msg) =
  msg 
formatDebugContextMarker filename inp ast m@(MarkMultiple msg markers) = 
  L.intercalate "\n" $ map (\line -> "          " ++ line) $ lines $ msg ++ "\n" ++ L.intercalate "\n" (map (formatDebugContextMarker filename inp ast) markers)
formatDebugContextMarker filename inp ast (MarkSegment msg debugMarkers) =
  let markers = mapMaybe (getMarker filename) debugMarkers in
  let diagnostic = addReport (addFile mempty filename inp :: Diagnostic String) (Err Nothing msg markers []) in
  let doc = prettyDiagnostic WithUnicode (TabSize 4) diagnostic in
  L.intercalate "\n" $ drop 2 $ map (drop 10) $ lines $ show doc
  where 
    getMarker :: String -> (P.Position, P.Position, String) -> Maybe (Position, Marker String)
    getMarker filename (start, end, msg) = 
      let (startSrc, endSrc) = (P.positionSrc start, P.positionSrc end) in
      if startSrc /= endSrc || startSrc /= filename then Nothing else Just $ ((Position (P.positionLC start) (P.positionLC $ findTokenEnd ast end) startSrc), Where msg)
