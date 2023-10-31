{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Instant.Parser.Types where

import Instant.Syntax
import Instant.Logs
import Error.Diagnose
import           Control.Monad
import           Control.Monad.Identity
import           Data.Void
import           Data.Void
import           qualified Text.Megaparsec as MP
import qualified Data.Text as T
import Error.Diagnose.Compat.Megaparsec

type Parser = MP.ParsecT Void String Identity

instance HasHints Void msg where
  hints _ = mempty