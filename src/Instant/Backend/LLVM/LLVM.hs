
module Instant.Backend.LLVM.LLVM(backend) where

import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Except
import qualified Data.Map as M
import           Data.Map(Map)
import qualified Data.Set as S
import           Data.Set(Set)
import           System.FilePath

import Instant.Backend.Base
import Instant.Syntax
import Instant.Backend.LLVM.Compiler

backend :: InstantBackend
backend = InstantBackend {
  name = "LLVM",
  inputExtension = "ll",
  run = \filename code -> do
      return $ evalState (runExceptT $ compileInstant code) (CompilerState 0 M.empty),
  compileExecutable = \filePath -> do
    let outpath = replaceExtension filePath "bc"
    execCmd "llvm-as" [filePath, "-o", outpath]
}