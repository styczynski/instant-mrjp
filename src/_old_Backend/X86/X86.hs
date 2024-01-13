module Backend.X86.X86 (backend) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer.Lazy
import Control.Monad.State.Strict
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Backend.Base
import Backend.X86.Compiler
import System.FilePath
import System.Directory
import qualified Data.Text as T
import Reporting.Logs

import qualified Backend.X86.Emitter as X86Emitter
import qualified Backend.X86.ASMOptimizer as X86Optimizer
import qualified Backend.X86.Formatter as X86Formatter
import qualified Linearized.Syntax as IR
import qualified Backend.X86.Syntax as ASM

compileCode :: String -> (IR.Program IR.IRPosition) -> ASMCompiler String
compileCode fileName code = do
  (asm, _) <- captureEmittedInstr $ X86Emitter.emit code
  (asm', _) <- captureEmittedInstr $ X86Optimizer.cleanupX86 asm
  X86Formatter.format ["lib/runtime.ext"] fileName asm

backend :: LatteBackend
backend =
  LatteBackend
    { backendName = "X86"
      , inputExtension = "asm"
      , run = \filename code@(IR.Program pos _ _ _) -> do
        (code, _) <- runWriterT (evalStateT (compileCode filename code) 0)
        return code
      , compileExecutable = \filePath -> do
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
