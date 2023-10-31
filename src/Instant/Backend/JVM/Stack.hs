module Instant.Backend.JVM.Stack where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Instant.Backend.JVM.Jasmine
import Instant.Syntax
import qualified Language.JVM.Common as J
import System.FilePath

estimateStackSize :: [J.Instruction] -> Int
estimateStackSize = maximum . scanl estimate 0
  where
    estimate :: Int -> J.Instruction -> Int
    estimate prev = \case
      J.Ldc _ -> prev + 1
      J.Istore _ -> prev - 1
      J.Iload _ -> prev + 1
      J.Iadd -> prev - 1
      J.Isub -> prev - 1
      J.Imul -> prev - 1
      J.Idiv -> prev - 1
      J.Swap -> prev
      J.Getstatic _ -> prev + 1
      J.Invokevirtual _ m -> prev - (length $ J.methodKeyParameterTypes m)
