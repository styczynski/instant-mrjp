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

data SimpleError = SimpleError {
    _errorName :: String
    , _errorDescription :: String
    , _errorSugestions :: [String]
    , _errorLocation :: Maybe Position
    , _errorContexts :: [(String, Maybe Position)]
    , _errorHelp :: Maybe (String, Position)
}

makeLensesWith abbreviatedFields ''SimpleError

