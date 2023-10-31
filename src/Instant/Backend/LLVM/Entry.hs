module Instant.Backend.LLVM.Entry where

entry :: String -> String
entry _fileName = "@.intprint = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1\ndeclare dso_local i32 @printf(i8*, ...) #1\n\ndefine dso_local i32 @main() {\n"