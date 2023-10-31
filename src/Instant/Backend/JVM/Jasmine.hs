module Instant.Backend.JVM.Jasmine where

import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Except
import qualified Data.Map as M
import           Data.Map(Map)
import qualified Data.Set as S
import           Data.Set(Set)
import           System.FilePath

import Instant.Syntax
import Data.Int
import Data.Word


import qualified Language.JVM.Common as J

serializeOp :: J.Instruction -> String
serializeOp = \case
  J.Iadd -> "iadd"
  J.Getstatic fieldId -> "getstatic " ++ (J.unClassName $ J.fieldIdClass fieldId) ++ " " ++ (J.fieldIdName fieldId)
  J.Invokevirtual _ m -> "invokevirtual " ++ (J.methodKeyName m)
  J.Isub -> "isub"
  J.Imul -> "imul"
  J.Idiv -> "idiv"
  J.Ldc (J.Integer (-1)) -> "iconst_m1"
  J.Ldc (J.Integer n) | n <= 5 -> "iconst_" ++ show n
  J.Ldc (J.Integer n) -> "bipush " ++ show n
  J.Iload n | n < 0 -> error $ "Bad load num: " <> show n
  J.Iload n | n <= 3 -> "iload_" ++ show n
  J.Iload n -> "iload " ++ show n
  J.Istore n | n < 0 -> error $ "Bad store num: " <> show n
  J.Istore n  | n <= 3 -> "istore_" ++ show n
  J.Istore n  -> "istore " ++ show n

intToWord16 :: Int -> Word16
intToWord16 n = fromIntegral n