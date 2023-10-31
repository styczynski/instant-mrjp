
module Instant.Backend.JVM.JVM(backend) where

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
import Instant.Backend.JVM.Jasmine
import Instant.Backend.JVM.Stack
import Instant.Backend.JVM.Compiler

import qualified Language.JVM.Common as J

backend :: InstantBackend
backend = InstantBackend {
  name = "JVM",
  inputExtension = "j",
  run = \filename code -> do
      return $ runReader (evalStateT (runExceptT $ compileInstant filename code) S.empty) (varMap code),
  compileExecutable = \filePath -> do
    let outpath = takeDirectory filePath
    execCmd "java" [ "-jar", "/home/students/inf/PUBLIC/MRJP/Jasmin/jasmin.jar", filePath, "-d", outpath]
}

