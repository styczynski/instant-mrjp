module Instant.Backend.LLVM.Syntax where

import qualified Data.List as L

data LLVMType
  = LLTInt Int
  | LLTPtr LLVMType
  | LLTVargs


i32 :: LLVMType
i32 = LLTInt 32


i8 :: LLVMType
i8 = LLTInt 8


ptr :: LLVMType -> LLVMType
ptr = LLTPtr


data LLVMLit
  = LLLReg String
  | LLLInt Int
  | LLLGetElementPtr (Int, LLVMType) String (LLVMType, LLVMLit) (LLVMType, LLVMLit)


data LLVMOp
  = LLOAssg String LLVMExpr
  | LLOCall LLVMType [LLVMType] String [(LLVMType, LLVMLit)]
  | LLORet LLVMType LLVMLit


data LLVMExpr
  = LLEAdd LLVMType LLVMLit LLVMLit
  | LLESub LLVMType LLVMLit LLVMLit
  | LLEMul LLVMType LLVMLit LLVMLit
  | LLIExprDiv LLVMType LLVMLit LLVMLit


type LLVM = [LLVMOp]


serializeLit :: LLVMLit -> String
serializeLit = \case
  LLLReg s -> "%" <> s
  LLLInt i -> show i
  LLLGetElementPtr (x, y) ref (t1, v1) (t2, v2) ->
    concat [ "getelementptr inbounds ("
           , "[", show x, " x ", serializeType y, "], "
           , "[", show x, " x ", serializeType y, "]* ", ref, ", "
           , serializeType t1, " ", serializeLit v1, ", "
           , serializeType t2, " ", serializeLit v2, ")"
           ]


serializeType :: LLVMType -> String
serializeType = \case
  LLTInt i -> "i" <> show i
  LLTPtr t -> serializeType t <> "*"
  LLTVargs -> "..."


serializeExpr :: LLVMExpr -> String
serializeExpr = \case
  LLEAdd t a b -> "add " <> serializeType t <> " "
                  <> serializeLit a <> ", "<> serializeLit b
  LLESub t a b -> "sub " <> serializeType t <> " "
                  <> serializeLit a <> ", "<> serializeLit b
  LLEMul t a b -> "mul " <> serializeType t <> " "
                  <> serializeLit a <> ", "<> serializeLit b
  LLIExprDiv t a b -> "sdiv " <> serializeType t <> " "
                  <> serializeLit a <> ", "<> serializeLit b


serializeOp :: LLVMOp -> String
serializeOp = \case
  LLOAssg s e -> "%" <> s <> " = " <> serializeExpr e
  LLOCall rett argst fun args -> concat
    [ "call "
    , serializeType rett, " "
    , bracSep (fmap serializeType argst) <> " "
    , fun
    , bracSep (fmap (\(t, l) -> serializeType t <> " " <> serializeLit l) args)
    ]
  LLORet t v -> "ret " <> serializeType t <> " " <> serializeLit v


bracSep :: [String] -> String
bracSep elems = '(' : concat (L.intersperse ", " elems) ++ ")"
