module Backend.X86.X86 (backend) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Backend.Base
import qualified Backend.X86.Compiler as Compiler
import System.FilePath

backend :: LatteBackend
backend =
  LatteBackend
    { backendName = "X86",
      inputExtension = "asm",
      run = \filename code -> do
        evalStateT (Compiler.compileCode filename code) 0,
      compileExecutable = \filePath -> do
        execCmd "/bin/bash" ["-c", "echo 'TODO: Provide compilation backend'"]
    }
