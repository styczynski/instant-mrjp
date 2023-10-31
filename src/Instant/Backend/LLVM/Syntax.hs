module Instant.Backend.LLVM.Syntax where

import Instant.Backend.Base
import qualified Data.List as L

data LLVMType
  =
    LINT Int
    | LVARARGS
    | LPOINTER LLVMType

lint32 :: LLVMType
lint32 = LINT 32

lint8 :: LLVMType
lint8 = LINT 8

ptr :: LLVMType -> LLVMType
ptr = LPOINTER

data LLVMLit
  = CONSTReg String
  | CONSTInt Int
  | CONSTGetElementPtr (Int, LLVMType) String (LLVMType, LLVMLit) (LLVMType, LLVMLit)

data LLVMOp
  = OPAssignment String LLVMExpr
  | OPDoCall LLVMType [LLVMType] String [(LLVMType, LLVMLit)]
  | OPDoReturn LLVMType LLVMLit

data LLVMExpr
  = INSTRAdd LLVMType LLVMLit LLVMLit
  | INSTRSub LLVMType LLVMLit LLVMLit
  | INSTRMul LLVMType LLVMLit LLVMLit
  | INSTRDiv LLVMType LLVMLit LLVMLit

type LLVM = [LLVMOp]

instance SerializableInstruction LLVMLit where
    toCode (CONSTReg s) = "%" <> s
    toCode (CONSTInt i) = show i
    toCode (CONSTGetElementPtr (x, y) ref (t1, v1) (t2, v2)) =
        concat
        [ "getelementptr inbounds (",
            "[",
            show x,
            " x ",
            toCode y,
            "], ",
            "[",
            show x,
            " x ",
            toCode y,
            "]* ",
            ref,
            ", ",
            toCode t1,
            " ",
            toCode v1,
            ", ",
            toCode t2,
            " ",
            toCode v2,
            ")"
        ]

instance SerializableInstruction LLVMType where
    toCode (LINT i) = "i" <> show i
    toCode (LVARARGS) = "..."
    toCode (LPOINTER t) = toCode t <> "*"

instance SerializableInstruction LLVMExpr where
  toCode (INSTRAdd t a b) =
    "add "
      <> toCode t
      <> " "
      <> toCode a
      <> ", "
      <> toCode b
  toCode (INSTRSub t a b) =
    "sub "
      <> toCode t
      <> " "
      <> toCode a
      <> ", "
      <> toCode b
  toCode (INSTRMul t a b) =
    "mul "
      <> toCode t
      <> " "
      <> toCode a
      <> ", "
      <> toCode b
  toCode (INSTRDiv t a b) =
    "sdiv "
      <> toCode t
      <> " "
      <> toCode a
      <> ", "
      <> toCode b

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
