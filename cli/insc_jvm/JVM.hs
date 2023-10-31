module Main where

import Instant
import qualified Instant.Backend.JVM.JVM as JVM

main :: IO ()
main = runCLI JVM.backend
