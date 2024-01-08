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
import System.Directory
import qualified Data.Text as T
import Reporting.Logs

backend :: LatteBackend
backend =
  LatteBackend
    { backendName = "X86",
      inputExtension = "asm",
      run = \filename code -> do
        evalStateT (Compiler.compileCode filename code) 0,
      compileExecutable = \filePath -> do
        let objName = replaceExtension filePath "o"
        let execName = takeBaseName filePath
        liftPipelineToBackend $ printLogInfo $ T.pack $ "Run NASM for X86 backend to generate '" ++ objName ++ "' from '" ++ filePath ++ "'"
        execCmd "nasm" [filePath, "-o", objName, "-f elf64"]
        liftPipelineToBackend $ printLogInfo $ T.pack $ "Run GCC for X86 backend to generate '" ++ execName ++ "' executable from object file '" ++ objName ++ "'"
        execCmd "gcc" ["-Lsrc/Runtime/dependencies/_built_/lib", objName, "lib/runtime", "-o", execName, "-l:libunistring.a"]
        liftPipelineToBackend $ printLogInfo $ T.pack $ "Cleanup leftover object file: '" ++ objName ++ "'"
        liftIO $ removeFile objName
        liftPipelineToBackend $ printLogInfo $ T.pack $ "X86 compilation of '" ++ filePath ++ "' seems to be successfull"
    }
