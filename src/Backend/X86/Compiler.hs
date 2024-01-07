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

type CompilerEnv = Int

type ASMCompiler a = (StateT (CompilerEnv) BackendPipeline) a

compileCode :: String -> (IR.Program IR.IRPosition) -> ASMCompiler String
compileCode fileName code = do
  return "nop"
