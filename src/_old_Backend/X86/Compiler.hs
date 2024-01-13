module Backend.X86.Compiler where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer.Lazy
import Control.Monad.State.Strict
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import System.FilePath
import Backend.Base
import Reporting.Logs

import qualified Linearized.Syntax as IR
import qualified Reporting.Errors.Def as Errors
import qualified Backend.X86.Syntax as ASM

type CompilerEnv = Int

type ASMCompiler a = (StateT (CompilerEnv) (WriterT [ASM.Instruction ASM.Position] BackendPipeline)) a

emitterLog :: String -> ASMCompiler ()
emitterLog msg = lift $ lift $ lift $ printLogInfoStr msg

emitInstr :: [ASM.Instruction ASM.Position] -> ASMCompiler ()
emitInstr = tell

captureEmittedInstr :: ASMCompiler a -> ASMCompiler ([ASM.Instruction ASM.Position], a)
captureEmittedInstr m = do
  s <- get
  (s', result, r) <- lift $ lift $ runInternal s m
  put s'
  return (result, r)
  where
    runInternal :: CompilerEnv -> (ASMCompiler a) -> BackendPipeline (CompilerEnv, [ASM.Instruction ASM.Position], a)
    runInternal s m = do
      ((r, state), instrs) <- runWriterT (runStateT m s)
      return (state, instrs, r)
