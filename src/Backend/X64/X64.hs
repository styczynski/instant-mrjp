module Backend.X64.X64 (backend) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer.Lazy
import Control.Monad.State.Strict
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Backend.Base
import System.FilePath
import System.Directory
import qualified Data.Text as T
import Reporting.Logs
import qualified IR.Compl as IR

-- import qualified Backend.X86.Emitter as X86Emitter
-- import qualified Backend.X86.ASMOptimizer as X86Optimizer
-- import qualified Backend.X86.Formatter as X86Formatter
-- import qualified Backend.X86.Syntax as ASM
import qualified Backend.X64.Generator as Generator
import qualified Linearized.Syntax as LSyntax

compileCode :: (Show a) => String -> (IR.CompiledProg a) -> BackendPipeline String
compileCode fileName (IR.CompiledProg pos meta cfgs) = do
  asm <- lift $ Generator.generate meta cfgs
  -- (asm, _) <- captureEmittedInstr $ X86Emitter.emit code
  -- (asm', _) <- captureEmittedInstr $ X86Optimizer.cleanupX86 asm
  --X86Formatter.format ["lib/runtime.ext"] fileName asm
  return asm

backend :: (Show a) => LatteBackend a
backend =
  LatteBackend
    { backendName = "X64"
      , inputExtension = "s"
      , run = \filename code -> do
        code <- compileCode filename code
        return code
      , compileExecutable = \filePath -> do
        let objName = replaceExtension filePath "o"
        let execName = (takeDirectory filePath) </> (takeBaseName filePath)
        --liftPipelineToBackend $ printLogInfo $ T.pack $ "Run NASM for X86 backend to generate '" ++ objName ++ "' from '" ++ filePath ++ "'"
        --execCmd "nasm" [filePath, "-o", objName, "-f elf64"]
        liftPipelineToBackend $ printLogInfo $ T.pack $ "Run GCC for X64 backend to generate '" ++ execName ++ "' executable from object file '" ++ objName ++ "'"
        execCmd "gcc" ["-fPIE", "-Lsrc/Runtime/dependencies/_built_/lib", "lib/runtime", "-l:libunistring.a", filePath, "-o", execName, "-z", "noexecstack"]
        --liftPipelineToBackend $ printLogInfo $ T.pack $ "Cleanup leftover object file: '" ++ objName ++ "'"
        --liftIO $ removeFile objName
        liftPipelineToBackend $ printLogInfo $ T.pack $ "X64 compilation of '" ++ filePath ++ "' seems to be successfull"
        return execName
    }
