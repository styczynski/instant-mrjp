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
    = LabelDef a Label
    | ADD64 a (Source' a) (Target' a)
    | AND64 a (Source' a) (Target' a)
    | CMP64 a (Source' a) (Target' a)
    | IDIV64 a (Source' a) (Target' a)
    | IMUL64 a (Source' a) (Target' a)
    | LEA64 a (Source' a) (Target' a)
    | MOV64 a (Source' a) (Target' a)
    | SUB64 a (Source' a) (Target' a)
    | TEST64 a (Source' a) (Target' a)
    | XOR64 a (Source' a) (Target' a)
    | XCHG64 a (Source' a) (Target' a)
    | SAL64 a (Source' a) (Target' a)
    | SAR64 a (Source' a) (Target' a)
    | ADD32 a (Source' a) (Target' a)
    | AND32 a (Source' a) (Target' a)
    | CMP32 a (Source' a) (Target' a)
    | IDIV32 a (Source' a) (Target' a)
    | IMUL32 a (Source' a) (Target' a)
    | LEA32 a (Source' a) (Target' a)
    | MOV32 a (Source' a) (Target' a)
    | SUB32 a (Source' a) (Target' a)
    | TEST32 a (Source' a) (Target' a)
    | XOR32 a (Source' a) (Target' a)
    | XCHG32 a (Source' a) (Target' a)
    | SAL32 a (Source' a) (Target' a)
    | SAR32 a (Source' a) (Target' a)
    | ADD16 a (Source' a) (Target' a)
    | AND16 a (Source' a) (Target' a)
    | CMP16 a (Source' a) (Target' a)
    | IDIV16 a (Source' a) (Target' a)
    | IMUL16 a (Source' a) (Target' a)
    | LEA16 a (Source' a) (Target' a)
    | MOV16 a (Source' a) (Target' a)
    | SUB16 a (Source' a) (Target' a)
    | TEST16 a (Source' a) (Target' a)
    | XOR16 a (Source' a) (Target' a)
    | XCHG16 a (Source' a) (Target' a)
    | SAL16 a (Source' a) (Target' a)
    | SAR16 a (Source' a) (Target' a)
    | NEG64 a (Target' a)
    | NEG32 a (Target' a)
    | NEG16 a (Target' a)
    | POP a (Reg' a)
    | PUSH a (Reg' a)
    | LEAVE a
    | RET a
    | CDQ a
    | SETE a (Reg' a)
    | SETG a (Reg' a)
    | SETGE a (Reg' a)
    | SETL a (Reg' a)
    | SETLE a (Reg' a)
    | SETNE a (Reg' a)
    | JMP a Label
    | JZ a Label
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable, C.Data, C.Typeable, C.Generic)

type Source = Source' BNFC'Position
data Source' a
    = FromConst a Integer
    | FromReg64 a (Reg' a)
    | FromMem64 a Integer (Reg' a)
    | FromReg32 a (Reg' a)
    | FromMem32 a Integer (Reg' a)
    | FromReg16 a (Reg' a)
    | FromMem16 a Integer (Reg' a)
    | FromReg8 a (Reg' a)
    | FromMem8 a Integer (Reg' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable, C.Data, C.Typeable, C.Generic)

type Target = Target' BNFC'Position
data Target' a
    = ToReg64 a (Reg' a)
    | ToMem64 a Integer (Reg' a)
    | ToReg32 a (Reg' a)
    | ToMem32 a Integer (Reg' a)
    | ToReg16 a (Reg' a)
    | ToMem16 a Integer (Reg' a)
    | ToReg8 a (Reg' a)
    | ToMem8 a Integer (Reg' a)
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
    LabelDef p _ -> p
    ADD64 p _ _ -> p
    AND64 p _ _ -> p
    CMP64 p _ _ -> p
    IDIV64 p _ _ -> p
    IMUL64 p _ _ -> p
    LEA64 p _ _ -> p
    MOV64 p _ _ -> p
    SUB64 p _ _ -> p
    TEST64 p _ _ -> p
    XOR64 p _ _ -> p
    XCHG64 p _ _ -> p
    SAL64 p _ _ -> p
    SAR64 p _ _ -> p
    ADD32 p _ _ -> p
    AND32 p _ _ -> p
    CMP32 p _ _ -> p
    IDIV32 p _ _ -> p
    IMUL32 p _ _ -> p
    LEA32 p _ _ -> p
    MOV32 p _ _ -> p
    SUB32 p _ _ -> p
    TEST32 p _ _ -> p
    XOR32 p _ _ -> p
    XCHG32 p _ _ -> p
    SAL32 p _ _ -> p
    SAR32 p _ _ -> p
    ADD16 p _ _ -> p
    AND16 p _ _ -> p
    CMP16 p _ _ -> p
    IDIV16 p _ _ -> p
    IMUL16 p _ _ -> p
    LEA16 p _ _ -> p
    MOV16 p _ _ -> p
    SUB16 p _ _ -> p
    TEST16 p _ _ -> p
    XOR16 p _ _ -> p
    XCHG16 p _ _ -> p
    SAL16 p _ _ -> p
    SAR16 p _ _ -> p
    NEG64 p _ -> p
    NEG32 p _ -> p
    NEG16 p _ -> p
    POP p _ -> p
    PUSH p _ -> p
    LEAVE p -> p
    RET p -> p
    CDQ p -> p
    SETE p _ -> p
    SETG p _ -> p
    SETGE p _ -> p
    SETL p _ -> p
    SETLE p _ -> p
    SETNE p _ -> p
    JMP p _ -> p
    JZ p _ -> p

instance HasPosition Source where
  hasPosition = \case
    FromConst p _ -> p
    FromReg64 p _ -> p
    FromMem64 p _ _ -> p
    FromReg32 p _ -> p
    FromMem32 p _ _ -> p
    FromReg16 p _ -> p
    FromMem16 p _ _ -> p
    FromReg8 p _ -> p
    FromMem8 p _ _ -> p

instance HasPosition Target where
  hasPosition = \case
    ToReg64 p _ -> p
    ToMem64 p _ _ -> p
    ToReg32 p _ -> p
    ToMem32 p _ _ -> p
    ToReg16 p _ -> p
    ToMem16 p _ _ -> p
    ToReg8 p _ -> p
    ToMem8 p _ _ -> p

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

