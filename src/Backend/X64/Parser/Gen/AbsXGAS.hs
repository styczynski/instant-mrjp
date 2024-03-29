-- File generated by the BNF Converter (bnfc 2.9.4).

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The abstract syntax of language X64GAS.

module Backend.X64.Parser.Gen.AbsXGAS where

import Prelude (Integer, String)
import qualified Prelude as C
  ( Eq, Ord, Show, Read
  , Functor, Foldable, Traversable
  , Int, Maybe(..)
  )
import qualified Data.String

import qualified Data.Data    as C (Data, Typeable)
import qualified GHC.Generics as C (Generic)

type AsmProgram = AsmProgram' BNFC'Position
data AsmProgram' a
    = AsmProgram a [Directive' a] (SectionData' a) (SectionCode' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable, C.Data, C.Typeable, C.Generic)

type SectionData = SectionData' BNFC'Position
data SectionData' a = SectionData a [AsmDataDef' a]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable, C.Data, C.Typeable, C.Generic)

type SectionCode = SectionCode' BNFC'Position
data SectionCode' a = SectionCode a [AsmInstr' a]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable, C.Data, C.Typeable, C.Generic)

type AsmDataDef = AsmDataDef' BNFC'Position
data AsmDataDef' a
    = AsmDataGlobal a Label | AsmDataDef a Label [Data' a]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable, C.Data, C.Typeable, C.Generic)

type CommentAnn = CommentAnn' BNFC'Position
data CommentAnn' a = Comment a CommentLike | NoComment a
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable, C.Data, C.Typeable, C.Generic)

type Data = Data' BNFC'Position
data Data' a
    = DataString a String
    | Data64 a (DataConst' a)
    | Data32 a (DataConst' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable, C.Data, C.Typeable, C.Generic)

type DataConst = DataConst' BNFC'Position
data DataConst' a = ConstInt a Integer | ConstLabel a Label
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable, C.Data, C.Typeable, C.Generic)

type Directive = Directive' BNFC'Position
data Directive' a = Extern a Label
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable, C.Data, C.Typeable, C.Generic)

type AsmInstr = AsmInstr' BNFC'Position
data AsmInstr' a
    = LabelDef a Label (CommentAnn' a)
    | ADD64 a (Source' a) (Target' a) (CommentAnn' a)
    | AND64 a (Source' a) (Target' a) (CommentAnn' a)
    | CMP64 a (Source' a) (Target' a) (CommentAnn' a)
    | IMUL64 a (Source' a) (Target' a) (CommentAnn' a)
    | LEA64 a (Source' a) (Target' a) (CommentAnn' a)
    | MOV64 a (Source' a) (Target' a) (CommentAnn' a)
    | SUB64 a (Source' a) (Target' a) (CommentAnn' a)
    | TEST64 a (Source' a) (Target' a) (CommentAnn' a)
    | XOR64 a (Source' a) (Target' a) (CommentAnn' a)
    | XCHG64 a (Source' a) (Target' a) (CommentAnn' a)
    | SAL64 a (Source' a) (Target' a) (CommentAnn' a)
    | SAR64 a (Source' a) (Target' a) (CommentAnn' a)
    | ADD32 a (Source' a) (Target' a) (CommentAnn' a)
    | AND32 a (Source' a) (Target' a) (CommentAnn' a)
    | CMP32 a (Source' a) (Target' a) (CommentAnn' a)
    | IMUL32 a (Source' a) (Target' a) (CommentAnn' a)
    | LEA32 a (Source' a) (Target' a) (CommentAnn' a)
    | MOV32 a (Source' a) (Target' a) (CommentAnn' a)
    | SUB32 a (Source' a) (Target' a) (CommentAnn' a)
    | TEST32 a (Source' a) (Target' a) (CommentAnn' a)
    | XOR32 a (Source' a) (Target' a) (CommentAnn' a)
    | XCHG32 a (Source' a) (Target' a) (CommentAnn' a)
    | SAL32 a (Source' a) (Target' a) (CommentAnn' a)
    | SAR32 a (Source' a) (Target' a) (CommentAnn' a)
    | ADD16 a (Source' a) (Target' a) (CommentAnn' a)
    | AND16 a (Source' a) (Target' a) (CommentAnn' a)
    | CMP16 a (Source' a) (Target' a) (CommentAnn' a)
    | IMUL16 a (Source' a) (Target' a) (CommentAnn' a)
    | LEA16 a (Source' a) (Target' a) (CommentAnn' a)
    | MOV16 a (Source' a) (Target' a) (CommentAnn' a)
    | SUB16 a (Source' a) (Target' a) (CommentAnn' a)
    | TEST16 a (Source' a) (Target' a) (CommentAnn' a)
    | XOR16 a (Source' a) (Target' a) (CommentAnn' a)
    | XCHG16 a (Source' a) (Target' a) (CommentAnn' a)
    | SAL16 a (Source' a) (Target' a) (CommentAnn' a)
    | SAR16 a (Source' a) (Target' a) (CommentAnn' a)
    | ADD8 a (Source' a) (Target' a) (CommentAnn' a)
    | AND8 a (Source' a) (Target' a) (CommentAnn' a)
    | CMP8 a (Source' a) (Target' a) (CommentAnn' a)
    | IMUL8 a (Source' a) (Target' a) (CommentAnn' a)
    | LEA8 a (Source' a) (Target' a) (CommentAnn' a)
    | MOV8 a (Source' a) (Target' a) (CommentAnn' a)
    | SUB8 a (Source' a) (Target' a) (CommentAnn' a)
    | TEST8 a (Source' a) (Target' a) (CommentAnn' a)
    | XOR8 a (Source' a) (Target' a) (CommentAnn' a)
    | XCHG8 a (Source' a) (Target' a) (CommentAnn' a)
    | SAL8 a (Source' a) (Target' a) (CommentAnn' a)
    | SAR8 a (Source' a) (Target' a) (CommentAnn' a)
    | NEG64 a (Target' a) (CommentAnn' a)
    | IDIV64 a (Target' a) (CommentAnn' a)
    | INC64 a (Target' a) (CommentAnn' a)
    | DEC64 a (Target' a) (CommentAnn' a)
    | NEG32 a (Target' a) (CommentAnn' a)
    | IDIV32 a (Target' a) (CommentAnn' a)
    | INC32 a (Target' a) (CommentAnn' a)
    | DEC32 a (Target' a) (CommentAnn' a)
    | NEG16 a (Target' a) (CommentAnn' a)
    | IDIV16 a (Target' a) (CommentAnn' a)
    | INC16 a (Target' a) (CommentAnn' a)
    | DEC16 a (Target' a) (CommentAnn' a)
    | NEG8 a (Target' a) (CommentAnn' a)
    | IDIV8 a (Target' a) (CommentAnn' a)
    | INC8 a (Target' a) (CommentAnn' a)
    | DEC8 a (Target' a) (CommentAnn' a)
    | PUSH64 a (Source' a) (CommentAnn' a)
    | PUSH32 a (Source' a) (CommentAnn' a)
    | PUSH16 a (Source' a) (CommentAnn' a)
    | PUSH8 a (Source' a) (CommentAnn' a)
    | CALL a Label (CommentAnn' a)
    | CALLINDIRECT a Integer (Reg' a) (CommentAnn' a)
    | POP a (Reg' a) (CommentAnn' a)
    | LEAVE a (CommentAnn' a)
    | RET a (CommentAnn' a)
    | CDQ a (CommentAnn' a)
    | SETE a (Reg' a) (CommentAnn' a)
    | SETG a (Reg' a) (CommentAnn' a)
    | SETGE a (Reg' a) (CommentAnn' a)
    | SETL a (Reg' a) (CommentAnn' a)
    | SETLE a (Reg' a) (CommentAnn' a)
    | SETNE a (Reg' a) (CommentAnn' a)
    | SETZ a (Reg' a) (CommentAnn' a)
    | SETNZ a (Reg' a) (CommentAnn' a)
    | JMP a Label (CommentAnn' a)
    | JE a Label (CommentAnn' a)
    | JG a Label (CommentAnn' a)
    | JGE a Label (CommentAnn' a)
    | JL a Label (CommentAnn' a)
    | JLE a Label (CommentAnn' a)
    | JNE a Label (CommentAnn' a)
    | JZ a Label (CommentAnn' a)
    | JNZ a Label (CommentAnn' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable, C.Data, C.Typeable, C.Generic)

type Source = Source' BNFC'Position
data Source' a
    = FromConst a ConstIntRef
    | FromReg64 a (Reg' a)
    | FromMem64 a Integer (Reg' a)
    | FromLabel64 a Label
    | FromLabelOffset64 a Label
    | FromMemComplex64 a Integer (Reg' a) (Reg' a) Integer
    | FromReg32 a (Reg' a)
    | FromMem32 a Integer (Reg' a)
    | FromLabel32 a Label
    | FromLabelOffset32 a Label
    | FromMemComplex32 a Integer (Reg' a) (Reg' a) Integer
    | FromReg16 a (Reg' a)
    | FromMem16 a Integer (Reg' a)
    | FromLabel16 a Label
    | FromLabelOffset16 a Label
    | FromMemComplex16 a Integer (Reg' a) (Reg' a) Integer
    | FromReg8 a (Reg' a)
    | FromMem8 a Integer (Reg' a)
    | FromLabel8 a Label
    | FromLabelOffset8 a Label
    | FromMemComplex8 a Integer (Reg' a) (Reg' a) Integer
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable, C.Data, C.Typeable, C.Generic)

type Target = Target' BNFC'Position
data Target' a
    = ToReg64 a (Reg' a)
    | ToMem64 a Integer (Reg' a)
    | ToMemComplex64 a Integer (Reg' a) (Reg' a) Integer
    | ToReg32 a (Reg' a)
    | ToMem32 a Integer (Reg' a)
    | ToMemComplex32 a Integer (Reg' a) (Reg' a) Integer
    | ToReg16 a (Reg' a)
    | ToMem16 a Integer (Reg' a)
    | ToMemComplex16 a Integer (Reg' a) (Reg' a) Integer
    | ToReg8 a (Reg' a)
    | ToMem8 a Integer (Reg' a)
    | ToMemComplex8 a Integer (Reg' a) (Reg' a) Integer
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable, C.Data, C.Typeable, C.Generic)

type Reg = Reg' BNFC'Position
data Reg' a
    = RAX a
    | RBX a
    | RCX a
    | RDX a
    | RDI a
    | RSI a
    | RSP a
    | RBP a
    | R8 a
    | R9 a
    | R10 a
    | R11 a
    | R12 a
    | R13 a
    | R14 a
    | R15 a
    | EAX a
    | EBX a
    | ECX a
    | EDX a
    | EDI a
    | ESI a
    | ESP a
    | EBP a
    | R8D a
    | R9D a
    | R10D a
    | R11D a
    | R12D a
    | R13D a
    | R14D a
    | R15D a
    | AX a
    | BX a
    | CX a
    | DX a
    | DI a
    | SI a
    | SP a
    | BP a
    | R8W a
    | R9W a
    | R10W a
    | R11W a
    | R12W a
    | R13W a
    | R14W a
    | R15W a
    | AL a
    | BL a
    | CL a
    | DL a
    | DIL a
    | SIL a
    | SPL a
    | BPL a
    | R8B a
    | R9B a
    | R10B a
    | R11B a
    | R12B a
    | R13B a
    | R14B a
    | R15B a
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable, C.Data, C.Typeable, C.Generic)

newtype CommentLike = CommentLike String
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic, Data.String.IsString)

newtype ConstIntRef = ConstIntRef String
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic, Data.String.IsString)

newtype Label = Label String
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic, Data.String.IsString)

-- | Start position (line, column) of something.

type BNFC'Position = C.Maybe (C.Int, C.Int)

pattern BNFC'NoPosition :: BNFC'Position
pattern BNFC'NoPosition = C.Nothing

pattern BNFC'Position :: C.Int -> C.Int -> BNFC'Position
pattern BNFC'Position line col = C.Just (line, col)

-- | Get the start position of something.

class HasPosition a where
  hasPosition :: a -> BNFC'Position

instance HasPosition AsmProgram where
  hasPosition = \case
    AsmProgram p _ _ _ -> p

instance HasPosition SectionData where
  hasPosition = \case
    SectionData p _ -> p

instance HasPosition SectionCode where
  hasPosition = \case
    SectionCode p _ -> p

instance HasPosition AsmDataDef where
  hasPosition = \case
    AsmDataGlobal p _ -> p
    AsmDataDef p _ _ -> p

instance HasPosition CommentAnn where
  hasPosition = \case
    Comment p _ -> p
    NoComment p -> p

instance HasPosition Data where
  hasPosition = \case
    DataString p _ -> p
    Data64 p _ -> p
    Data32 p _ -> p

instance HasPosition DataConst where
  hasPosition = \case
    ConstInt p _ -> p
    ConstLabel p _ -> p

instance HasPosition Directive where
  hasPosition = \case
    Extern p _ -> p

instance HasPosition AsmInstr where
  hasPosition = \case
    LabelDef p _ _ -> p
    ADD64 p _ _ _ -> p
    AND64 p _ _ _ -> p
    CMP64 p _ _ _ -> p
    IMUL64 p _ _ _ -> p
    LEA64 p _ _ _ -> p
    MOV64 p _ _ _ -> p
    SUB64 p _ _ _ -> p
    TEST64 p _ _ _ -> p
    XOR64 p _ _ _ -> p
    XCHG64 p _ _ _ -> p
    SAL64 p _ _ _ -> p
    SAR64 p _ _ _ -> p
    ADD32 p _ _ _ -> p
    AND32 p _ _ _ -> p
    CMP32 p _ _ _ -> p
    IMUL32 p _ _ _ -> p
    LEA32 p _ _ _ -> p
    MOV32 p _ _ _ -> p
    SUB32 p _ _ _ -> p
    TEST32 p _ _ _ -> p
    XOR32 p _ _ _ -> p
    XCHG32 p _ _ _ -> p
    SAL32 p _ _ _ -> p
    SAR32 p _ _ _ -> p
    ADD16 p _ _ _ -> p
    AND16 p _ _ _ -> p
    CMP16 p _ _ _ -> p
    IMUL16 p _ _ _ -> p
    LEA16 p _ _ _ -> p
    MOV16 p _ _ _ -> p
    SUB16 p _ _ _ -> p
    TEST16 p _ _ _ -> p
    XOR16 p _ _ _ -> p
    XCHG16 p _ _ _ -> p
    SAL16 p _ _ _ -> p
    SAR16 p _ _ _ -> p
    ADD8 p _ _ _ -> p
    AND8 p _ _ _ -> p
    CMP8 p _ _ _ -> p
    IMUL8 p _ _ _ -> p
    LEA8 p _ _ _ -> p
    MOV8 p _ _ _ -> p
    SUB8 p _ _ _ -> p
    TEST8 p _ _ _ -> p
    XOR8 p _ _ _ -> p
    XCHG8 p _ _ _ -> p
    SAL8 p _ _ _ -> p
    SAR8 p _ _ _ -> p
    NEG64 p _ _ -> p
    IDIV64 p _ _ -> p
    INC64 p _ _ -> p
    DEC64 p _ _ -> p
    NEG32 p _ _ -> p
    IDIV32 p _ _ -> p
    INC32 p _ _ -> p
    DEC32 p _ _ -> p
    NEG16 p _ _ -> p
    IDIV16 p _ _ -> p
    INC16 p _ _ -> p
    DEC16 p _ _ -> p
    NEG8 p _ _ -> p
    IDIV8 p _ _ -> p
    INC8 p _ _ -> p
    DEC8 p _ _ -> p
    PUSH64 p _ _ -> p
    PUSH32 p _ _ -> p
    PUSH16 p _ _ -> p
    PUSH8 p _ _ -> p
    CALL p _ _ -> p
    CALLINDIRECT p _ _ _ -> p
    POP p _ _ -> p
    LEAVE p _ -> p
    RET p _ -> p
    CDQ p _ -> p
    SETE p _ _ -> p
    SETG p _ _ -> p
    SETGE p _ _ -> p
    SETL p _ _ -> p
    SETLE p _ _ -> p
    SETNE p _ _ -> p
    SETZ p _ _ -> p
    SETNZ p _ _ -> p
    JMP p _ _ -> p
    JE p _ _ -> p
    JG p _ _ -> p
    JGE p _ _ -> p
    JL p _ _ -> p
    JLE p _ _ -> p
    JNE p _ _ -> p
    JZ p _ _ -> p
    JNZ p _ _ -> p

instance HasPosition Source where
  hasPosition = \case
    FromConst p _ -> p
    FromReg64 p _ -> p
    FromMem64 p _ _ -> p
    FromLabel64 p _ -> p
    FromLabelOffset64 p _ -> p
    FromMemComplex64 p _ _ _ _ -> p
    FromReg32 p _ -> p
    FromMem32 p _ _ -> p
    FromLabel32 p _ -> p
    FromLabelOffset32 p _ -> p
    FromMemComplex32 p _ _ _ _ -> p
    FromReg16 p _ -> p
    FromMem16 p _ _ -> p
    FromLabel16 p _ -> p
    FromLabelOffset16 p _ -> p
    FromMemComplex16 p _ _ _ _ -> p
    FromReg8 p _ -> p
    FromMem8 p _ _ -> p
    FromLabel8 p _ -> p
    FromLabelOffset8 p _ -> p
    FromMemComplex8 p _ _ _ _ -> p

instance HasPosition Target where
  hasPosition = \case
    ToReg64 p _ -> p
    ToMem64 p _ _ -> p
    ToMemComplex64 p _ _ _ _ -> p
    ToReg32 p _ -> p
    ToMem32 p _ _ -> p
    ToMemComplex32 p _ _ _ _ -> p
    ToReg16 p _ -> p
    ToMem16 p _ _ -> p
    ToMemComplex16 p _ _ _ _ -> p
    ToReg8 p _ -> p
    ToMem8 p _ _ -> p
    ToMemComplex8 p _ _ _ _ -> p

instance HasPosition Reg where
  hasPosition = \case
    RAX p -> p
    RBX p -> p
    RCX p -> p
    RDX p -> p
    RDI p -> p
    RSI p -> p
    RSP p -> p
    RBP p -> p
    R8 p -> p
    R9 p -> p
    R10 p -> p
    R11 p -> p
    R12 p -> p
    R13 p -> p
    R14 p -> p
    R15 p -> p
    EAX p -> p
    EBX p -> p
    ECX p -> p
    EDX p -> p
    EDI p -> p
    ESI p -> p
    ESP p -> p
    EBP p -> p
    R8D p -> p
    R9D p -> p
    R10D p -> p
    R11D p -> p
    R12D p -> p
    R13D p -> p
    R14D p -> p
    R15D p -> p
    AX p -> p
    BX p -> p
    CX p -> p
    DX p -> p
    DI p -> p
    SI p -> p
    SP p -> p
    BP p -> p
    R8W p -> p
    R9W p -> p
    R10W p -> p
    R11W p -> p
    R12W p -> p
    R13W p -> p
    R14W p -> p
    R15W p -> p
    AL p -> p
    BL p -> p
    CL p -> p
    DL p -> p
    DIL p -> p
    SIL p -> p
    SPL p -> p
    BPL p -> p
    R8B p -> p
    R9B p -> p
    R10B p -> p
    R11B p -> p
    R12B p -> p
    R13B p -> p
    R14B p -> p
    R15B p -> p

