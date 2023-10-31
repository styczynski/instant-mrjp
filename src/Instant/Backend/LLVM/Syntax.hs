module Instant.Backend.LLVM.Syntax where

import qualified Data.List as L
import Instant.Backend.Base

data LLVMType
  = LINT Int
  | LVARARGS
  | LPOINTER LLVMType


data LLVMLit
  = CONSTReg String
  | CONSTInt Int
  | CONSTGetElementPtr (Int, LLVMType) String (LLVMType, LLVMLit) (LLVMType, LLVMLit)

data LLVMPrintFunc
  = FNPRINTF
  | FNINTPRINT

lint32 :: LLVMType
lint32 = LINT 32

lint8 :: LLVMType
lint8 = LINT 8

ptr :: LLVMType -> LLVMType
ptr = LPOINTER


data LLVMOp
  = OPAssignment String LLVMExpr
  | OPDoCall LLVMType [LLVMType] String [(LLVMType, LLVMLit)]
  | OPDoReturn LLVMType LLVMLit

data LLVMExpr
  = INSTRAdd LLVMType LLVMLit LLVMLit
  | INSTRSub LLVMType LLVMLit LLVMLit
  | INSTRMul LLVMType LLVMLit LLVMLit
  | INSTRDiv LLVMType LLVMLit LLVMLit

instance SerializableInstruction LLVMPrintFunc where
  toCode FNINTPRINT = "@.intprint"
  toCode FNPRINTF = "@printf"

constZero :: LLVMLit
constZero = CONSTInt 0

toCodeStdExpr :: String -> LLVMType -> LLVMLit -> LLVMLit -> String
toCodeStdExpr prefix valueType arg1 arg2 = prefix ++ " " ++ (toCode valueType) ++ " " ++ (toCode arg1) ++ ", " ++ (toCode arg2)

instance SerializableInstruction LLVMExpr where
  toCode (INSTRAdd argType arg1 arg2) = toCodeStdExpr "add" argType arg1 arg2
  toCode (INSTRMul argType arg1 arg2) = toCodeStdExpr "mul" argType arg1 arg2
  toCode (INSTRSub argType arg1 arg2) = toCodeStdExpr "sub" argType arg1 arg2
  toCode (INSTRDiv argType arg1 arg2) = toCodeStdExpr "sdiv" argType arg1 arg2

instance SerializableInstruction LLVMType where
  toCode (LINT i) = "i" ++ (show i)
  toCode (LPOINTER payload) = (toCode payload) ++ "*"
  toCode (LVARARGS) = "..."

instance SerializableInstruction LLVMLit where
  toCode (CONSTReg s) = "%" <> s
  toCode (CONSTInt i) = show i
  toCode (CONSTGetElementPtr (x, y) ref (typeA, valueA) (typeB, valueB)) =
    "getelementptr inbounds ([" ++ (show x) ++ " x " ++ (toCode y) ++ "], [" ++ (show x) ++ " x " ++ (toCode y) ++ "]* " ++ (ref) ++ ", " ++ (toCode typeA) ++ " " ++ (toCode valueA) ++ ", " ++ (toCode typeB) ++ " " ++ (toCode valueB) ++ ")"


instance SerializableInstruction LLVMOp where
  toCode (OPAssignment s e) = "%" <> s <> " = " <> toCode e
  toCode (OPDoCall rett argst fun args) =
    concat
      [ "call ",
        toCode rett,
        " ",
        bracSep (fmap toCode argst) <> " ",
        fun,
        bracSep (fmap (\(t, l) -> toCode t <> " " <> toCode l) args)
      ]
  toCode (OPDoReturn t v) = "ret " <> toCode t <> " " <> toCode v

bracSep :: [String] -> String
bracSep elems = '(' : concat (L.intersperse ", " elems) ++ ")"
