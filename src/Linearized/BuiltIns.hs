module Linearized.BuiltIns(builtIns, builtInsLabels) where

import qualified Linearized.Syntax as B
import Reporting.Errors.Position



builtIns =
    [
        B.Fun BuiltIn (B.Label BuiltIn "_Array_toString") (B.Reference BuiltIn $ B.Label BuiltIn "Array") [] [],
        B.Fun BuiltIn (B.Label BuiltIn "_Object_toString") (B.Reference BuiltIn $ B.Label BuiltIn "Object") [] [],
        B.Fun BuiltIn (B.Label BuiltIn "_Object_getHashCode") (B.IntT BuiltIn) [] [],
        B.Fun BuiltIn (B.Label BuiltIn "_Object_equals") (B.ByteT BuiltIn) [] [],
        B.Fun BuiltIn (B.Label BuiltIn "_String_equals") (B.ByteT BuiltIn) [] [],
        B.Fun BuiltIn (B.Label BuiltIn "_String_getHashCode") (B.IntT BuiltIn) [] [],
        B.Fun BuiltIn (B.Label BuiltIn "_String_toString") (B.Reference BuiltIn $ B.Label BuiltIn "String") [] [],
        B.Fun BuiltIn (B.Label BuiltIn "_String_substring") (B.Reference BuiltIn $ B.Label BuiltIn "String") [] [],
        B.Fun BuiltIn (B.Label BuiltIn "_String_length") (B.IntT BuiltIn) [] [],
        B.Fun BuiltIn (B.Label BuiltIn "_String_indexOf") (B.IntT BuiltIn) [] [],
        B.Fun BuiltIn (B.Label BuiltIn "_String_getBytes") (B.Reference BuiltIn $ B.Label BuiltIn "String") [] [],
        B.Fun BuiltIn (B.Label BuiltIn "_String_endsWith") (B.ByteT BuiltIn) [] [],
        B.Fun BuiltIn (B.Label BuiltIn "_String_startsWith") (B.ByteT BuiltIn) [] [],
        B.Fun BuiltIn (B.Label BuiltIn "_String_concat") (B.Reference BuiltIn $ B.Label BuiltIn "String") [(B.Reference BuiltIn $ B.Label BuiltIn "String", B.Name BuiltIn "_1"), (B.Reference BuiltIn $ B.Label BuiltIn "String", B.Name BuiltIn "_2")] [],
        B.Fun BuiltIn (B.Label BuiltIn "_String_charAt") (B.IntT BuiltIn) [] [],
        B.Fun BuiltIn (B.Label BuiltIn "printString") (B.ByteT BuiltIn) [(B.Reference BuiltIn $ B.Label BuiltIn "String", B.Name BuiltIn "_1")] [],
        B.Fun BuiltIn (B.Label BuiltIn "printInt") (B.ByteT BuiltIn) [(B.IntT BuiltIn, B.Name BuiltIn "_1")] [],
        B.Fun BuiltIn (B.Label BuiltIn "printByte") (B.ByteT BuiltIn) [(B.ByteT BuiltIn, B.Name BuiltIn "_1")] [],
        B.Fun BuiltIn (B.Label BuiltIn "printBoolean") (B.ByteT BuiltIn) [] [],
        B.Fun BuiltIn (B.Label BuiltIn "printBinArray") (B.ByteT BuiltIn) [] [],
        B.Fun BuiltIn (B.Label BuiltIn "byteToString") (B.Reference BuiltIn $ B.Label BuiltIn "String") [] [],
        B.Fun BuiltIn (B.Label BuiltIn "boolToString") (B.Reference BuiltIn $ B.Label BuiltIn "String") [] [],
        B.Fun BuiltIn (B.Label BuiltIn "intToString") (B.Reference BuiltIn $ B.Label BuiltIn "String") [] [],
        B.Fun BuiltIn (B.Label BuiltIn "print") (B.ByteT BuiltIn) [] [],
        B.Fun BuiltIn (B.Label BuiltIn "error") (B.ByteT BuiltIn) [] [],
        B.Fun BuiltIn (B.Label BuiltIn "readInt") (B.IntT BuiltIn) [] [],
        B.Fun BuiltIn (B.Label BuiltIn "readString") (B.Reference BuiltIn $ B.Label BuiltIn "String") [] []
    ]

builtInsLabels = map (\(B.Fun p (B.Label _ name) _ _ _) -> name) builtIns