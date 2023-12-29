{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE  OverloadedStrings #-}
module Reporting.Errors.Errors where

import Reporting.Errors.Def
import Reporting.Errors.Messages
import Reporting.Errors.Base as Err
import Reporting.Logs

import Data.Monoid

import Parser.Types
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Reporting.Errors.Position as P

import Control.Lens
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.List.NonEmpty as NEL

import Error.Diagnose
import Data.Maybe
import Data.Text.IO (hPutStr)
import Prettyprinter --(Doc, Pretty, hardline, pretty, defaultLayoutOptions, reAnnotateS, layoutPretty)
import Prettyprinter.Render.Terminal (renderStrict, AnsiStyle, Color (..), bold, color, colorDull)

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

mzip :: (Monoid b) => [a] -> [b] -> a -> b -> [(a, b)]
mzip (a:as) (b:bs) defa defb = (a, b) : mzip as bs defa defb
mzip [] (b:bs) defa defb = (defa, b) : mzip [] bs defa defb
mzip (a:as) [] defa defb = (a, defb) : mzip as [] defa defb
mzip _ _ _ _ = []

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
    NoMarker -> return ()
    markers -> printDebugContextMarkers filename inp ast markers
  where
    printDebugContextMarkers :: String -> String -> RawProgram -> DebugContextMarker -> LattePipeline ()
    printDebugContextMarkers filename inp ast marker =
      let showDoc = liftIO . hPutStr stdout . renderStrict . reAnnotateS defaultStyle . layoutPretty defaultLayoutOptions in
      let innerDoc = (nest 6 (formatDebugContextMarker filename inp ast marker <> "\n\n\n")) in
      let divider = (annotate (OtherStyle $ bold <> color Black) (pageWidth (\(AvailablePerLine l r) -> pretty $ T.replicate 12 " " <> T.replicate (div l 2) "─"))) in
      showDoc $ divider <> line <> innerDoc
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

formatDebugContextMarker :: String -> String -> RawProgram -> DebugContextMarker -> Doc (Annotation AnsiStyle)
formatDebugContextMarker _ _ _ NoMarker = emptyDoc
formatDebugContextMarker filename inp ast (MarkerGroup markers) = 
  vcat $ map (\m -> formatDebugContextMarker filename inp ast m <> hardline) markers
formatDebugContextMarker filename inp ast m@(MarkNothing msg) =
  annotate (OtherStyle $ bold <> color Black) $ pretty msg
formatDebugContextMarker filename inp ast m@(MarkMultiple msg markers) = 
  let leftBarStyle = annotate (OtherStyle $ color Black) in
  let mergedDoc = foldl (\acc e -> acc <> line <> e) emptyDoc (map (formatDebugContextMarker filename inp ast) markers) in
  let renderDocLines = T.lines . renderStrict . reAnnotateS defaultStyle . layoutPretty defaultLayoutOptions in
  vcat $ map (\(mc, line) -> mc <+> pretty line) $ [(leftBarStyle $ pretty $ T.pack "        │", T.pack "")] ++ mzip ((L.map (\mc -> (leftBarStyle $ pretty (T.pack [mc]) <> " │ ")) $ " " ++ msg)) (renderDocLines mergedDoc) ((leftBarStyle $ pretty $ T.pack "  │ ")) (T.pack "")
  --T.intercalate "\n" $ map (\line -> "      " <> line) $ T.lines $ (T.pack $ "    " ++ msg) <> "\n" <> T.intercalate "\n" (map (formatDebugContextMarker filename inp ast) markers)
formatDebugContextMarker filename inp ast (MarkSegment msg debugMarkers) =
  let markers = mapMaybe (getMarker filename) debugMarkers in
  let diagnostic = addReport (addFile mempty filename inp :: Diagnostic String) (Err Nothing msg markers []) in
  --let showDiagnostic = renderStrict . reAnnotateS defaultStyle . layoutPretty defaultLayoutOptions . prettyDiagnostic' WithUnicode (TabSize 4) . fmap pretty in
  let codeDoc = (prettyDiagnostic' WithUnicode (TabSize 4) . fmap pretty) diagnostic in
  let renderDocLines = T.lines . renderStrict . reAnnotateS defaultStyle . layoutPretty defaultLayoutOptions in
  let cutCodeDoc = vcat . map pretty . drop 3 . map ((<>) " ") . map (T.drop 20) . renderDocLines in
  if null msg then (cutCodeDoc codeDoc) else (annotate (OtherStyle $ bold <> color Black) $ pretty $ "    " ++ msg) <> line <> (cutCodeDoc codeDoc)
  --T.intercalate "\n" $ drop 2 $ map (T.drop 19) $ T.lines $ showDiagnostic diagnostic
  where 
    getMarker :: String -> (P.Position, P.Position, String) -> Maybe (Position, Marker String)
    getMarker filename (start, end, msg) = 
      let (startSrc, endSrc) = (P.positionSrc start, P.positionSrc end) in
      if startSrc /= endSrc || startSrc /= filename then Nothing else Just $ ((Position (P.positionLC start) (P.positionLC $ findTokenEnd ast end) startSrc), Where msg)
