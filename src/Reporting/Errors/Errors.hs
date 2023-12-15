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
  case origin of
    Nothing -> return ()
    Just errLocation -> do
      markers <- return $ [(Position (P.positionLC errLocation) (P.positionLC $ findTokenEnd ast errLocation) (P.positionSrc errLocation), This description)]
      helpMarkers <- return $ case (simpleError^.Err.help) of
        Nothing -> []
        (Just (label, p)) -> [(Position (P.positionLC p) (P.positionLC $ findTokenEnd ast p) (P.positionSrc p), Maybe $ "Action: " ++ label)]
      contextMarkers <- return $ L.map (mapContext $ P.positionSrc errLocation) $ simpleError^.Err.contexts
      -- sourceClassMarkers <- return $ case simpleError^.Err.oocontext of
      --   Nothing -> []
      --   Just clsName -> [(Position (line, 1) (line, col) origFilename, Where clsName)] ++ contextMarkers
      suggestions <- return $ case (simpleError^.Err.sugestions) of
        [] -> []
        lines -> [Note $ "This that can potentially help:\n" ++ lineMap lines id]
      diagnostic <- return $ addReport (addFile mempty filename inp :: Diagnostic String) (Err Nothing title (markers ++ contextMarkers ++ helpMarkers) ([] ++ suggestions))
      printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle diagnostic
  where
    mapContext :: String -> (String, Maybe P.Position) -> (Position, Marker String)
    mapContext origFilename (label, labelLoc) = case labelLoc of
      Nothing -> (Position (1, 1) (1, 1) origFilename, Where label)
      Just p -> (Position (P.positionLC p) (P.positionLC $ findTokenEnd ast p) (P.positionSrc p), Where label)

  