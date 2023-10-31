module Instant.Backend.JVM.Jasmine where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Int
import Instant.Backend.Base
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Word
import Instant.Syntax
import qualified Language.JVM.Common as J
import System.FilePath

instance SerializableInstruction J.Instruction where
  toCode (J.Iadd) = "iadd"
  toCode (J.Getstatic fieldId) = "getstatic " ++ (J.unClassName $ J.fieldIdClass fieldId) ++ " " ++ (J.fieldIdName fieldId)
  toCode (J.Invokevirtual _ m) = "invokevirtual " ++ (J.methodKeyName m)
  toCode (J.Isub) = "isub"
  toCode (J.Imul) = "imul"
  toCode (J.Idiv) = "idiv"
  toCode (J.Ldc k) = case k of
    (J.Integer (-1)) -> "iconst_m1"
    (J.Integer n) | n <= 5 -> "iconst_" ++ show n
    (J.Integer n) -> "bipush " ++ show n
  toCode (J.Iload n) = case n of
    _ | n < 0 -> error $ "Bad load num: " <> show n
    _ | n <= 3 -> "iload_" ++ show n
    _ -> "iload " ++ show n
  toCode (J.Istore n) = case n of
    _ | n < 0 -> error $ "Bad store num: " <> show n
    _ | n <= 3 -> "istore_" ++ show n
    _ -> "istore " ++ show n

intToWord16 :: Int -> Word16
intToWord16 n = fromIntegral n
