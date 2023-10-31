module Instant.Backend.LLVM.LLVM (backend) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Instant.Backend.Base
import Instant.Backend.LLVM.Compiler
import Instant.Syntax
import System.FilePath

backend :: InstantBackend
backend =
  InstantBackend
    { name = "LLVM",
      inputExtension = "ll",
      run = \filename code -> do
        return $ evalState (runExceptT $ compileInstant code) (CompilerState 0 M.empty),
      compileExecutable = \filePath -> do
        let outpath = replaceExtension filePath "bc"
        execCmd "llvm-as" [filePath, "-o", outpath]
    }
