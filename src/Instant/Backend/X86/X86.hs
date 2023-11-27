module Instant.Backend.X86.X86 (backend) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Instant.Backend.Base
import Instant.Backend.X86.Compiler
import Instant.Backend.X86.Jasmine
import Instant.Backend.X86.Stack
import Instant.Syntax
import qualified Language.JVM.Common as J
import System.FilePath

backend :: InstantBackend
backend =
  InstantBackend
    { name = "X86",
      inputExtension = "j",
      run = \filename code -> do
        return $ runReader (evalStateT (runExceptT $ compileICode filename code) S.empty) (varMap code),
      compileExecutable = \filePath -> do
        let outpath = takeDirectory filePath
        execCmd "java" ["-jar", "/home/students/inf/PUBLIC/MRJP/Jasmin/jasmin.jar", filePath, "-d", outpath]
    }
