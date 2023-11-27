module Main where

import Instant
import qualified Instant.Backend.X86.X86 as X86

main :: IO ()
main = runCLI X86.backend
