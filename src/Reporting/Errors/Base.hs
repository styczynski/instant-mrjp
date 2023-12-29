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
module Reporting.Errors.Base where
import Control.Lens

import Reporting.Errors.Position

data DebugContextMarker = 
    MarkSegment String [(Position, Position, String)]
    | MarkMultiple String [DebugContextMarker]
    | MarkNothing String
    | NoMarker
    | MarkerGroup [DebugContextMarker]
  deriving (Eq, Show)

_combineMarkers :: DebugContextMarker -> DebugContextMarker -> DebugContextMarker
_combineMarkers marker NoMarker = marker
_combineMarkers NoMarker marker = marker
_combineMarkers (MarkerGroup markers) marker = MarkerGroup $ markers ++ [marker]
_combineMarkers marker (MarkerGroup markers) = MarkerGroup (marker:markers)
_combineMarkers markerA markerB = MarkerGroup [markerA, markerB]

combineMarkers :: [DebugContextMarker] -> DebugContextMarker
combineMarkers = foldl _combineMarkers NoMarker

data SimpleError = SimpleError {
    _errorName :: String
    , _errorDescription :: String
    , _errorSugestions :: [String]
    , _errorLocation :: Maybe Position
    , _errorContexts :: [(String, Maybe Position)]
    , _errorHelp :: Maybe (String, Position)
    , _errorMarkers :: DebugContextMarker
}

makeLensesWith abbreviatedFields ''SimpleError

