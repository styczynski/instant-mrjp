module Linearized.BuiltIns(builtIns, builtInsLabels) where

import qualified Linearized.Syntax as B
import Reporting.Errors.Position

clsM :: String -> Maybe (B.Label Position)
clsM n = Just $ (B.Label BuiltIn n)

builtIns =
    [
        B.Fun BuiltIn (clsM "Array") (B.Label BuiltIn "toString") (B.Reference BuiltIn $ B.Label BuiltIn "Array") [] [],
        B.Fun BuiltIn (clsM "Object") (B.Label BuiltIn "toString") (B.Reference BuiltIn $ B.Label BuiltIn "Object") [] [],
        B.Fun BuiltIn (clsM "Object") (B.Label BuiltIn "getHashCode") (B.IntT BuiltIn) [] [],
        B.Fun BuiltIn (clsM "Object") (B.Label BuiltIn "equals") (B.ByteT BuiltIn) [] [],
        B.Fun BuiltIn (clsM "String") (B.Label BuiltIn "equals") (B.ByteT BuiltIn) [] [],
        B.Fun BuiltIn (clsM "String") (B.Label BuiltIn "getHashCode") (B.IntT BuiltIn) [] [],
        B.Fun BuiltIn (clsM "String") (B.Label BuiltIn "toString") (B.Reference BuiltIn $ B.Label BuiltIn "String") [] [],
        B.Fun BuiltIn (clsM "String") (B.Label BuiltIn "substring") (B.Reference BuiltIn $ B.Label BuiltIn "String") [] [],
        B.Fun BuiltIn (clsM "String") (B.Label BuiltIn "length") (B.IntT BuiltIn) [] [],
        B.Fun BuiltIn (clsM "String") (B.Label BuiltIn "indexOf") (B.IntT BuiltIn) [] [],
        B.Fun BuiltIn (clsM "String") (B.Label BuiltIn "getBytes") (B.Reference BuiltIn $ B.Label BuiltIn "String") [] [],
        B.Fun BuiltIn (clsM "String") (B.Label BuiltIn "endsWith") (B.ByteT BuiltIn) [] [],
        B.Fun BuiltIn (clsM "String") (B.Label BuiltIn "startsWith") (B.ByteT BuiltIn) [] [],
        B.Fun BuiltIn (clsM "String") (B.Label BuiltIn "concat") (B.Reference BuiltIn $ B.Label BuiltIn "String") [(B.Reference BuiltIn $ B.Label BuiltIn "String", B.Name BuiltIn "_1"), (B.Reference BuiltIn $ B.Label BuiltIn "String", B.Name BuiltIn "_2")] [],
        B.Fun BuiltIn (clsM "String") (B.Label BuiltIn "charAt") (B.IntT BuiltIn) [] [],
        B.Fun BuiltIn Nothing (B.Label BuiltIn "printString") (B.ByteT BuiltIn) [(B.Reference BuiltIn $ B.Label BuiltIn "String", B.Name BuiltIn "_1")] [],
        B.Fun BuiltIn Nothing (B.Label BuiltIn "printInt") (B.ByteT BuiltIn) [(B.IntT BuiltIn, B.Name BuiltIn "_1")] [],
        B.Fun BuiltIn Nothing (B.Label BuiltIn "printByte") (B.ByteT BuiltIn) [(B.ByteT BuiltIn, B.Name BuiltIn "_1")] [],
        B.Fun BuiltIn Nothing (B.Label BuiltIn "printBoolean") (B.ByteT BuiltIn) [] [],
        B.Fun BuiltIn Nothing (B.Label BuiltIn "printBinArray") (B.ByteT BuiltIn) [] [],
        B.Fun BuiltIn Nothing (B.Label BuiltIn "byteToString") (B.Reference BuiltIn $ B.Label BuiltIn "String") [] [],
        B.Fun BuiltIn Nothing (B.Label BuiltIn "boolToString") (B.Reference BuiltIn $ B.Label BuiltIn "String") [] [],
        B.Fun BuiltIn Nothing (B.Label BuiltIn "intToString") (B.Reference BuiltIn $ B.Label BuiltIn "String") [] [],
        B.Fun BuiltIn Nothing (B.Label BuiltIn "print") (B.ByteT BuiltIn) [] [],
        B.Fun BuiltIn Nothing (B.Label BuiltIn "error") (B.ByteT BuiltIn) [] [],
        B.Fun BuiltIn Nothing (B.Label BuiltIn "readInt") (B.IntT BuiltIn) [] [],
        B.Fun BuiltIn Nothing (B.Label BuiltIn "readString") (B.Reference BuiltIn $ B.Label BuiltIn "String") [] []
    ]

builtInsLabels = map (\(B.Fun p cls name _ _ _) -> getLabel cls name) builtIns
    where
        getLabel (Nothing) (B.Label _ name) = name
        getLabel (Just (B.Label _ cls)) (B.Label _ name) = "_" ++ cls ++ "_" ++ name