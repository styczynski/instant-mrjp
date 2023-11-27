module Instant.Backend.X86.Compiler where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Instant.Syntax
import System.FilePath
import Instant.Backend.Base

type CompilerM = ExceptT String (StateT (Set String) (Reader String))

compileICode :: String -> ICode -> CompilerM String
compileICode fileName code = do
  return "nop"
