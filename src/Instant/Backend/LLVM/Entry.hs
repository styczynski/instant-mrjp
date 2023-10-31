module Instant.Backend.LLVM.Entry where

entry :: String -> String
entry _fileName =
  unlines
    [ "@.intprint = private unnamed_addr constant [4 x lint8] c\"%d\\0A\\00\", align 1",
      "declare dso_local lint32 @printf(lint8*, ...) #1",
      "",
      "define dso_local lint32 @main() {\n"
    ]