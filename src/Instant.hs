module Instant(parse, toJVM, toLLVM) where

import           Instant.Parse
import qualified Instant.JVM as JVM
import qualified Instant.LLVM as LLVM

import Instant.Syntax


parse :: String -> String -> Either String ICode
parse filename code = toInstantNode <$> parseInstant filename code


toJVM :: String -> ICode -> Either String String
toJVM = JVM.build


toLLVM :: ICode -> Either String String
toLLVM = LLVM.build
