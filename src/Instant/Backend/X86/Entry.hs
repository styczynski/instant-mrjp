module Instant.Backend.X86.Entry where
import System.FilePath

entry :: String -> String
entry fileName = ".source " ++ fileName ++ "\n.class public " ++ takeBaseName fileName ++ "\n.super java/lang/Object\n\n.method public <init>()V\n  aload_0\n  invokenonvirtual java/lang/Object/<init>()V" ++ "\n  return" ++ "\n.end method"