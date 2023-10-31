module Instant.Backend.JVM.Entry where
import System.FilePath

entry :: String -> String
entry fileName =
  unlines
    [ ".source " ++ fileName,
      ".class public " ++ takeBaseName fileName,
      ".super java/lang/Object",
      "",
      ".method public <init>()V",
      "  aload_0",
      "  invokenonvirtual java/lang/Object/<init>()V",
      "  return",
      ".end method"
    ]