module Backend.X86.Compiler where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import System.FilePath
import Backend.Base

import qualified Linearized.Syntax as IR
import qualified Reporting.Errors.Def as Errors
import qualified Backend.X86.Syntax as X

import qualified Backend.X86.Emitter as X86Emitter
import qualified Backend.X86.ASMOptimizer as X86Optimizer

type CompilerEnv = Int

type ASMCompiler a = (StateT (CompilerEnv) BackendPipeline) a

compileCode :: String -> (IR.Program IR.IRPosition) -> ASMCompiler String
compileCode fileName code = do
  asm <- return $ X86Emitter.emit code
  asm' <- return $ X86Optimizer.cleanupX86 asm
  return $ show asm'