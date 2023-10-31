
module Instant.Backend.JVM.Stack where

import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Except
import qualified Data.Map as M
import           Data.Map(Map)
import qualified Data.Set as S
import           Data.Set(Set)
import           System.FilePath

import Instant.Syntax
import Instant.Backend.JVM.Jasmine


import qualified Language.JVM.Common as J

estimateStackSize :: [J.Instruction] -> Int
estimateStackSize = maximum . scanl estimate 0 where
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
