{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Instant.Parser.Types where

import Control.Monad
import Control.Monad.Identity
import qualified Data.Text as T
import Data.Void
import Error.Diagnose
import Error.Diagnose.Compat.Megaparsec
import Instant.Logs
import Instant.Syntax
import qualified Text.Megaparsec as MP

type Parser = MP.ParsecT Void String Identity

instance HasHints Void msg where
  hints _ = mempty
