module Main where

import Instant
import qualified Instant.Backend.LLVM as LLVM

main :: IO ()
main = runCLI LLVM.backend