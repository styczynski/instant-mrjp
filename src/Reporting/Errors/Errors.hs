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

import Control.Lens
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.List.NonEmpty as NEL

import Error.Diagnose

import Prelude hiding ((<>))

class Errorable a where
  describe :: a -> Err.SimpleError
  getOrigin :: a -> Maybe (Int, Int)
  getSourceName :: a -> String

instance Errorable Error where
  describe e = decodeError e
  getOrigin _ = Nothing
  getSourceName _ = ""

-- instance Errorable ErrorPack where
--   describe (ErrorPack pack) = let (_, err) = NEL.head pack in describe err
--   getOrigin (ErrorPack pack) = let (errorMeta, _) = NEL.head pack in errorMeta

numberedLineStr :: (String -> String) -> (Int, String) -> String
numberedLineStr fn (no, line) = "  " ++ (show no) ++ ".) " ++ (fn line)

lineMap :: [String] -> (String -> String) -> String
lineMap l fn = let numberedLines = zip [1..(L.length l)] l in L.intercalate "\n" (L.map (numberedLineStr fn) numberedLines)

printErrors :: (Errorable err) => err -> String -> String -> LattePipeline ()
printErrors errorable filename inp = do
  simpleError <- return $ describe errorable
  title <- return $ "Problem: " ++ (simpleError^.Err.name)
  description <- return $ simpleError^.description
  origin <- return $ getOrigin errorable
  case origin of
    Nothing -> return ()
    (Just (line, col)) -> do
      markers <- return $ [(Position (line, 1) (line, col) (getSourceName errorable), This description)]
      sourceClassMarkers <- return $ case simpleError^.Err.oocontext of
        Nothing -> []
        Just clsName -> [(Position (line, 1) (line, col) (getSourceName errorable), Where clsName)]
      suggestions <- return $ case (simpleError^.Err.sugestions) of
        [] -> []
        lines -> [Note $ "This that can potentially help:\n" ++ lineMap lines id]
      diagnostic <- return $ addReport (addFile mempty filename inp :: Diagnostic String) (Err Nothing title (markers ++ sourceClassMarkers) ([] ++ suggestions))
      printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle diagnostic