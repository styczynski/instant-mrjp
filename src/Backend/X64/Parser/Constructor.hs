
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
module Backend.X64.Parser.Constructor where

import Data.Generics.Product
import Data.Generics.Sum
import GHC.Generics (Generic)

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer.Lazy

import qualified Backend.X64.Parser.Gen.AbsXGAS as Syntax

-- Assembly generator definition
type ASMGenerator a v = (WriterT (GeneratorOut a) (Except String)) v

data GeneratorOut a = GeneratorOut [Instr a] [Syntax.AsmDataDef' a]

instance Semigroup (GeneratorOut a) where
GeneratorOut instr1 defs1 <> GeneratorOut instr2 defs2 = GeneratorOut (instr1 <> instr2) (defs1 <> defs2)

instance Monoid (GeneratorOut a) where
mempty = GeneratorOut [] []

_emitInstr :: a -> Instr a -> ASMGenerator a ()
_emitInstr pos instr = tell $ GeneratorOut [instr] []

_emitDef :: a -> Syntax.AsmDataDef' a -> ASMGenerator a ()
_emitDef pos def = tell $ GeneratorOut [] [def]

-- Registers

data Reg64 = RAX| RBX| RCX| RDX| RDI| RSI| RSP| RBP| R8| R9| R10| R11| R12| R13| R14| R15
	deriving (Eq, Ord, Show, Read, Generic)


data Reg32 = EAX| EBX| ECX| EDX| EDI| ESI| ESP| EBP| R8D| R9D| R10D| R11D| R12D| R13D| R14D| R15D
	deriving (Eq, Ord, Show, Read, Generic)


data Reg16 = AX| BX| CX| DX| DI| SI| SP| BP| R8W| R9W| R10W| R11W| R12W| R13W| R14W| R15W
	deriving (Eq, Ord, Show, Read, Generic)


data Reg8 = AL| BL| CL| DL| DIL| SIL| SPL| BPL| R8B| R9B| R10B| R11B| R12B| R13B| R14B| R15B
	deriving (Eq, Ord, Show, Read, Generic)


-- Size classes

data SIZE64 = SIZE64
	deriving (Eq, Ord, Show, Read, Generic)


data SIZE32 = SIZE32
	deriving (Eq, Ord, Show, Read, Generic)


data SIZE16 = SIZE16
	deriving (Eq, Ord, Show, Read, Generic)


data SIZE8 = SIZE8
	deriving (Eq, Ord, Show, Read, Generic)


class IsTarget target size target64 target32 target16 target8 | target -> size target64 target32 target16 target8 where
	to64 :: target -> target64
	to32 :: target -> target32
	to16 :: target -> target16
	to8 :: target -> target8
	asSource :: a -> target -> Syntax.Source' a
	asTarget :: a -> target -> Syntax.Target' a
	sizeOf :: target -> Int


instance IsTarget Reg64 SIZE64 Reg64 Reg32 Reg16 Reg8 where
	to64 RAX = RAX
	to64 RBX = RBX
	to64 RCX = RCX
	to64 RDX = RDX
	to64 RDI = RDI
	to64 RSI = RSI
	to64 RSP = RSP
	to64 RBP = RBP
	to64 R8 = R8
	to64 R9 = R9
	to64 R10 = R10
	to64 R11 = R11
	to64 R12 = R12
	to64 R13 = R13
	to64 R14 = R14
	to64 R15 = R15
	to32 RAX = EAX
	to32 RBX = EBX
	to32 RCX = ECX
	to32 RDX = EDX
	to32 RDI = EDI
	to32 RSI = ESI
	to32 RSP = ESP
	to32 RBP = EBP
	to32 R8 = R8D
	to32 R9 = R9D
	to32 R10 = R10D
	to32 R11 = R11D
	to32 R12 = R12D
	to32 R13 = R13D
	to32 R14 = R14D
	to32 R15 = R15D
	to16 RAX = AX
	to16 RBX = BX
	to16 RCX = CX
	to16 RDX = DX
	to16 RDI = DI
	to16 RSI = SI
	to16 RSP = SP
	to16 RBP = BP
	to16 R8 = R8W
	to16 R9 = R9W
	to16 R10 = R10W
	to16 R11 = R11W
	to16 R12 = R12W
	to16 R13 = R13W
	to16 R14 = R14W
	to16 R15 = R15W
	to8 RAX = AL
	to8 RBX = BL
	to8 RCX = CL
	to8 RDX = DL
	to8 RDI = DIL
	to8 RSI = SIL
	to8 RSP = SPL
	to8 RBP = BPL
	to8 R8 = R8B
	to8 R9 = R9B
	to8 R10 = R10B
	to8 R11 = R11B
	to8 R12 = R12B
	to8 R13 = R13B
	to8 R14 = R14B
	to8 R15 = R15B
	asSource pos RAX = Syntax.FromReg64 pos $ Syntax.RAX pos
	asSource pos RBX = Syntax.FromReg64 pos $ Syntax.RBX pos
	asSource pos RCX = Syntax.FromReg64 pos $ Syntax.RCX pos
	asSource pos RDX = Syntax.FromReg64 pos $ Syntax.RDX pos
	asSource pos RDI = Syntax.FromReg64 pos $ Syntax.RDI pos
	asSource pos RSI = Syntax.FromReg64 pos $ Syntax.RSI pos
	asSource pos RSP = Syntax.FromReg64 pos $ Syntax.RSP pos
	asSource pos RBP = Syntax.FromReg64 pos $ Syntax.RBP pos
	asSource pos R8 = Syntax.FromReg64 pos $ Syntax.R8 pos
	asSource pos R9 = Syntax.FromReg64 pos $ Syntax.R9 pos
	asSource pos R10 = Syntax.FromReg64 pos $ Syntax.R10 pos
	asSource pos R11 = Syntax.FromReg64 pos $ Syntax.R11 pos
	asSource pos R12 = Syntax.FromReg64 pos $ Syntax.R12 pos
	asSource pos R13 = Syntax.FromReg64 pos $ Syntax.R13 pos
	asSource pos R14 = Syntax.FromReg64 pos $ Syntax.R14 pos
	asSource pos R15 = Syntax.FromReg64 pos $ Syntax.R15 pos
	asTarget pos RAX = Syntax.ToReg64 pos $ Syntax.RAX pos
	asTarget pos RBX = Syntax.ToReg64 pos $ Syntax.RBX pos
	asTarget pos RCX = Syntax.ToReg64 pos $ Syntax.RCX pos
	asTarget pos RDX = Syntax.ToReg64 pos $ Syntax.RDX pos
	asTarget pos RDI = Syntax.ToReg64 pos $ Syntax.RDI pos
	asTarget pos RSI = Syntax.ToReg64 pos $ Syntax.RSI pos
	asTarget pos RSP = Syntax.ToReg64 pos $ Syntax.RSP pos
	asTarget pos RBP = Syntax.ToReg64 pos $ Syntax.RBP pos
	asTarget pos R8 = Syntax.ToReg64 pos $ Syntax.R8 pos
	asTarget pos R9 = Syntax.ToReg64 pos $ Syntax.R9 pos
	asTarget pos R10 = Syntax.ToReg64 pos $ Syntax.R10 pos
	asTarget pos R11 = Syntax.ToReg64 pos $ Syntax.R11 pos
	asTarget pos R12 = Syntax.ToReg64 pos $ Syntax.R12 pos
	asTarget pos R13 = Syntax.ToReg64 pos $ Syntax.R13 pos
	asTarget pos R14 = Syntax.ToReg64 pos $ Syntax.R14 pos
	asTarget pos R15 = Syntax.ToReg64 pos $ Syntax.R15 pos
	sizeOf _ = 64


instance IsTarget Reg32 SIZE32 Reg64 Reg32 Reg16 Reg8 where
	to64 EAX = RAX
	to64 EBX = RBX
	to64 ECX = RCX
	to64 EDX = RDX
	to64 EDI = RDI
	to64 ESI = RSI
	to64 ESP = RSP
	to64 EBP = RBP
	to64 R8D = R8
	to64 R9D = R9
	to64 R10D = R10
	to64 R11D = R11
	to64 R12D = R12
	to64 R13D = R13
	to64 R14D = R14
	to64 R15D = R15
	to32 EAX = EAX
	to32 EBX = EBX
	to32 ECX = ECX
	to32 EDX = EDX
	to32 EDI = EDI
	to32 ESI = ESI
	to32 ESP = ESP
	to32 EBP = EBP
	to32 R8D = R8D
	to32 R9D = R9D
	to32 R10D = R10D
	to32 R11D = R11D
	to32 R12D = R12D
	to32 R13D = R13D
	to32 R14D = R14D
	to32 R15D = R15D
	to16 EAX = AX
	to16 EBX = BX
	to16 ECX = CX
	to16 EDX = DX
	to16 EDI = DI
	to16 ESI = SI
	to16 ESP = SP
	to16 EBP = BP
	to16 R8D = R8W
	to16 R9D = R9W
	to16 R10D = R10W
	to16 R11D = R11W
	to16 R12D = R12W
	to16 R13D = R13W
	to16 R14D = R14W
	to16 R15D = R15W
	to8 EAX = AL
	to8 EBX = BL
	to8 ECX = CL
	to8 EDX = DL
	to8 EDI = DIL
	to8 ESI = SIL
	to8 ESP = SPL
	to8 EBP = BPL
	to8 R8D = R8B
	to8 R9D = R9B
	to8 R10D = R10B
	to8 R11D = R11B
	to8 R12D = R12B
	to8 R13D = R13B
	to8 R14D = R14B
	to8 R15D = R15B
	asSource pos EAX = Syntax.FromReg32 pos $ Syntax.EAX pos
	asSource pos EBX = Syntax.FromReg32 pos $ Syntax.EBX pos
	asSource pos ECX = Syntax.FromReg32 pos $ Syntax.ECX pos
	asSource pos EDX = Syntax.FromReg32 pos $ Syntax.EDX pos
	asSource pos EDI = Syntax.FromReg32 pos $ Syntax.EDI pos
	asSource pos ESI = Syntax.FromReg32 pos $ Syntax.ESI pos
	asSource pos ESP = Syntax.FromReg32 pos $ Syntax.ESP pos
	asSource pos EBP = Syntax.FromReg32 pos $ Syntax.EBP pos
	asSource pos R8D = Syntax.FromReg32 pos $ Syntax.R8D pos
	asSource pos R9D = Syntax.FromReg32 pos $ Syntax.R9D pos
	asSource pos R10D = Syntax.FromReg32 pos $ Syntax.R10D pos
	asSource pos R11D = Syntax.FromReg32 pos $ Syntax.R11D pos
	asSource pos R12D = Syntax.FromReg32 pos $ Syntax.R12D pos
	asSource pos R13D = Syntax.FromReg32 pos $ Syntax.R13D pos
	asSource pos R14D = Syntax.FromReg32 pos $ Syntax.R14D pos
	asSource pos R15D = Syntax.FromReg32 pos $ Syntax.R15D pos
	asTarget pos EAX = Syntax.ToReg32 pos $ Syntax.EAX pos
	asTarget pos EBX = Syntax.ToReg32 pos $ Syntax.EBX pos
	asTarget pos ECX = Syntax.ToReg32 pos $ Syntax.ECX pos
	asTarget pos EDX = Syntax.ToReg32 pos $ Syntax.EDX pos
	asTarget pos EDI = Syntax.ToReg32 pos $ Syntax.EDI pos
	asTarget pos ESI = Syntax.ToReg32 pos $ Syntax.ESI pos
	asTarget pos ESP = Syntax.ToReg32 pos $ Syntax.ESP pos
	asTarget pos EBP = Syntax.ToReg32 pos $ Syntax.EBP pos
	asTarget pos R8D = Syntax.ToReg32 pos $ Syntax.R8D pos
	asTarget pos R9D = Syntax.ToReg32 pos $ Syntax.R9D pos
	asTarget pos R10D = Syntax.ToReg32 pos $ Syntax.R10D pos
	asTarget pos R11D = Syntax.ToReg32 pos $ Syntax.R11D pos
	asTarget pos R12D = Syntax.ToReg32 pos $ Syntax.R12D pos
	asTarget pos R13D = Syntax.ToReg32 pos $ Syntax.R13D pos
	asTarget pos R14D = Syntax.ToReg32 pos $ Syntax.R14D pos
	asTarget pos R15D = Syntax.ToReg32 pos $ Syntax.R15D pos
	sizeOf _ = 32


instance IsTarget Reg16 SIZE16 Reg64 Reg32 Reg16 Reg8 where
	to64 AX = RAX
	to64 BX = RBX
	to64 CX = RCX
	to64 DX = RDX
	to64 DI = RDI
	to64 SI = RSI
	to64 SP = RSP
	to64 BP = RBP
	to64 R8W = R8
	to64 R9W = R9
	to64 R10W = R10
	to64 R11W = R11
	to64 R12W = R12
	to64 R13W = R13
	to64 R14W = R14
	to64 R15W = R15
	to32 AX = EAX
	to32 BX = EBX
	to32 CX = ECX
	to32 DX = EDX
	to32 DI = EDI
	to32 SI = ESI
	to32 SP = ESP
	to32 BP = EBP
	to32 R8W = R8D
	to32 R9W = R9D
	to32 R10W = R10D
	to32 R11W = R11D
	to32 R12W = R12D
	to32 R13W = R13D
	to32 R14W = R14D
	to32 R15W = R15D
	to16 AX = AX
	to16 BX = BX
	to16 CX = CX
	to16 DX = DX
	to16 DI = DI
	to16 SI = SI
	to16 SP = SP
	to16 BP = BP
	to16 R8W = R8W
	to16 R9W = R9W
	to16 R10W = R10W
	to16 R11W = R11W
	to16 R12W = R12W
	to16 R13W = R13W
	to16 R14W = R14W
	to16 R15W = R15W
	to8 AX = AL
	to8 BX = BL
	to8 CX = CL
	to8 DX = DL
	to8 DI = DIL
	to8 SI = SIL
	to8 SP = SPL
	to8 BP = BPL
	to8 R8W = R8B
	to8 R9W = R9B
	to8 R10W = R10B
	to8 R11W = R11B
	to8 R12W = R12B
	to8 R13W = R13B
	to8 R14W = R14B
	to8 R15W = R15B
	asSource pos AX = Syntax.FromReg16 pos $ Syntax.AX pos
	asSource pos BX = Syntax.FromReg16 pos $ Syntax.BX pos
	asSource pos CX = Syntax.FromReg16 pos $ Syntax.CX pos
	asSource pos DX = Syntax.FromReg16 pos $ Syntax.DX pos
	asSource pos DI = Syntax.FromReg16 pos $ Syntax.DI pos
	asSource pos SI = Syntax.FromReg16 pos $ Syntax.SI pos
	asSource pos SP = Syntax.FromReg16 pos $ Syntax.SP pos
	asSource pos BP = Syntax.FromReg16 pos $ Syntax.BP pos
	asSource pos R8W = Syntax.FromReg16 pos $ Syntax.R8W pos
	asSource pos R9W = Syntax.FromReg16 pos $ Syntax.R9W pos
	asSource pos R10W = Syntax.FromReg16 pos $ Syntax.R10W pos
	asSource pos R11W = Syntax.FromReg16 pos $ Syntax.R11W pos
	asSource pos R12W = Syntax.FromReg16 pos $ Syntax.R12W pos
	asSource pos R13W = Syntax.FromReg16 pos $ Syntax.R13W pos
	asSource pos R14W = Syntax.FromReg16 pos $ Syntax.R14W pos
	asSource pos R15W = Syntax.FromReg16 pos $ Syntax.R15W pos
	asTarget pos AX = Syntax.ToReg16 pos $ Syntax.AX pos
	asTarget pos BX = Syntax.ToReg16 pos $ Syntax.BX pos
	asTarget pos CX = Syntax.ToReg16 pos $ Syntax.CX pos
	asTarget pos DX = Syntax.ToReg16 pos $ Syntax.DX pos
	asTarget pos DI = Syntax.ToReg16 pos $ Syntax.DI pos
	asTarget pos SI = Syntax.ToReg16 pos $ Syntax.SI pos
	asTarget pos SP = Syntax.ToReg16 pos $ Syntax.SP pos
	asTarget pos BP = Syntax.ToReg16 pos $ Syntax.BP pos
	asTarget pos R8W = Syntax.ToReg16 pos $ Syntax.R8W pos
	asTarget pos R9W = Syntax.ToReg16 pos $ Syntax.R9W pos
	asTarget pos R10W = Syntax.ToReg16 pos $ Syntax.R10W pos
	asTarget pos R11W = Syntax.ToReg16 pos $ Syntax.R11W pos
	asTarget pos R12W = Syntax.ToReg16 pos $ Syntax.R12W pos
	asTarget pos R13W = Syntax.ToReg16 pos $ Syntax.R13W pos
	asTarget pos R14W = Syntax.ToReg16 pos $ Syntax.R14W pos
	asTarget pos R15W = Syntax.ToReg16 pos $ Syntax.R15W pos
	sizeOf _ = 16


instance IsTarget Reg8 SIZE8 Reg64 Reg32 Reg16 Reg8 where
	to64 AL = RAX
	to64 BL = RBX
	to64 CL = RCX
	to64 DL = RDX
	to64 DIL = RDI
	to64 SIL = RSI
	to64 SPL = RSP
	to64 BPL = RBP
	to64 R8B = R8
	to64 R9B = R9
	to64 R10B = R10
	to64 R11B = R11
	to64 R12B = R12
	to64 R13B = R13
	to64 R14B = R14
	to64 R15B = R15
	to32 AL = EAX
	to32 BL = EBX
	to32 CL = ECX
	to32 DL = EDX
	to32 DIL = EDI
	to32 SIL = ESI
	to32 SPL = ESP
	to32 BPL = EBP
	to32 R8B = R8D
	to32 R9B = R9D
	to32 R10B = R10D
	to32 R11B = R11D
	to32 R12B = R12D
	to32 R13B = R13D
	to32 R14B = R14D
	to32 R15B = R15D
	to16 AL = AX
	to16 BL = BX
	to16 CL = CX
	to16 DL = DX
	to16 DIL = DI
	to16 SIL = SI
	to16 SPL = SP
	to16 BPL = BP
	to16 R8B = R8W
	to16 R9B = R9W
	to16 R10B = R10W
	to16 R11B = R11W
	to16 R12B = R12W
	to16 R13B = R13W
	to16 R14B = R14W
	to16 R15B = R15W
	to8 AL = AL
	to8 BL = BL
	to8 CL = CL
	to8 DL = DL
	to8 DIL = DIL
	to8 SIL = SIL
	to8 SPL = SPL
	to8 BPL = BPL
	to8 R8B = R8B
	to8 R9B = R9B
	to8 R10B = R10B
	to8 R11B = R11B
	to8 R12B = R12B
	to8 R13B = R13B
	to8 R14B = R14B
	to8 R15B = R15B
	asSource pos AL = Syntax.FromReg8 pos $ Syntax.AL pos
	asSource pos BL = Syntax.FromReg8 pos $ Syntax.BL pos
	asSource pos CL = Syntax.FromReg8 pos $ Syntax.CL pos
	asSource pos DL = Syntax.FromReg8 pos $ Syntax.DL pos
	asSource pos DIL = Syntax.FromReg8 pos $ Syntax.DIL pos
	asSource pos SIL = Syntax.FromReg8 pos $ Syntax.SIL pos
	asSource pos SPL = Syntax.FromReg8 pos $ Syntax.SPL pos
	asSource pos BPL = Syntax.FromReg8 pos $ Syntax.BPL pos
	asSource pos R8B = Syntax.FromReg8 pos $ Syntax.R8B pos
	asSource pos R9B = Syntax.FromReg8 pos $ Syntax.R9B pos
	asSource pos R10B = Syntax.FromReg8 pos $ Syntax.R10B pos
	asSource pos R11B = Syntax.FromReg8 pos $ Syntax.R11B pos
	asSource pos R12B = Syntax.FromReg8 pos $ Syntax.R12B pos
	asSource pos R13B = Syntax.FromReg8 pos $ Syntax.R13B pos
	asSource pos R14B = Syntax.FromReg8 pos $ Syntax.R14B pos
	asSource pos R15B = Syntax.FromReg8 pos $ Syntax.R15B pos
	asTarget pos AL = Syntax.ToReg8 pos $ Syntax.AL pos
	asTarget pos BL = Syntax.ToReg8 pos $ Syntax.BL pos
	asTarget pos CL = Syntax.ToReg8 pos $ Syntax.CL pos
	asTarget pos DL = Syntax.ToReg8 pos $ Syntax.DL pos
	asTarget pos DIL = Syntax.ToReg8 pos $ Syntax.DIL pos
	asTarget pos SIL = Syntax.ToReg8 pos $ Syntax.SIL pos
	asTarget pos SPL = Syntax.ToReg8 pos $ Syntax.SPL pos
	asTarget pos BPL = Syntax.ToReg8 pos $ Syntax.BPL pos
	asTarget pos R8B = Syntax.ToReg8 pos $ Syntax.R8B pos
	asTarget pos R9B = Syntax.ToReg8 pos $ Syntax.R9B pos
	asTarget pos R10B = Syntax.ToReg8 pos $ Syntax.R10B pos
	asTarget pos R11B = Syntax.ToReg8 pos $ Syntax.R11B pos
	asTarget pos R12B = Syntax.ToReg8 pos $ Syntax.R12B pos
	asTarget pos R13B = Syntax.ToReg8 pos $ Syntax.R13B pos
	asTarget pos R14B = Syntax.ToReg8 pos $ Syntax.R14B pos
	asTarget pos R15B = Syntax.ToReg8 pos $ Syntax.R15B pos
	sizeOf _ = 8


-- Instruction wrappers

data Instr a = Instr a (Syntax.AsmInstr' a) (Maybe (String))
	deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor)

_wrap :: a -> Syntax.AsmInstr' a -> Instr a
_wrap pos instr = Instr pos instr Nothing

_instr_add :: Int -> (a -> Syntax.Source' a -> Syntax.Target' a -> Syntax.AsmInstr' a)
_instr_add 64 = Syntax.ADD64
_instr_add 32 = Syntax.ADD32
_instr_add 16 = Syntax.ADD16
_instr_and :: Int -> (a -> Syntax.Source' a -> Syntax.Target' a -> Syntax.AsmInstr' a)
_instr_and 64 = Syntax.AND64
_instr_and 32 = Syntax.AND32
_instr_and 16 = Syntax.AND16
_instr_cmp :: Int -> (a -> Syntax.Source' a -> Syntax.Target' a -> Syntax.AsmInstr' a)
_instr_cmp 64 = Syntax.CMP64
_instr_cmp 32 = Syntax.CMP32
_instr_cmp 16 = Syntax.CMP16
_instr_idiv :: Int -> (a -> Syntax.Source' a -> Syntax.Target' a -> Syntax.AsmInstr' a)
_instr_idiv 64 = Syntax.IDIV64
_instr_idiv 32 = Syntax.IDIV32
_instr_idiv 16 = Syntax.IDIV16
_instr_imul :: Int -> (a -> Syntax.Source' a -> Syntax.Target' a -> Syntax.AsmInstr' a)
_instr_imul 64 = Syntax.IMUL64
_instr_imul 32 = Syntax.IMUL32
_instr_imul 16 = Syntax.IMUL16
_instr_lea :: Int -> (a -> Syntax.Source' a -> Syntax.Target' a -> Syntax.AsmInstr' a)
_instr_lea 64 = Syntax.LEA64
_instr_lea 32 = Syntax.LEA32
_instr_lea 16 = Syntax.LEA16
_instr_mov :: Int -> (a -> Syntax.Source' a -> Syntax.Target' a -> Syntax.AsmInstr' a)
_instr_mov 64 = Syntax.MOV64
_instr_mov 32 = Syntax.MOV32
_instr_mov 16 = Syntax.MOV16
_instr_sub :: Int -> (a -> Syntax.Source' a -> Syntax.Target' a -> Syntax.AsmInstr' a)
_instr_sub 64 = Syntax.SUB64
_instr_sub 32 = Syntax.SUB32
_instr_sub 16 = Syntax.SUB16
_instr_test :: Int -> (a -> Syntax.Source' a -> Syntax.Target' a -> Syntax.AsmInstr' a)
_instr_test 64 = Syntax.TEST64
_instr_test 32 = Syntax.TEST32
_instr_test 16 = Syntax.TEST16
_instr_xor :: Int -> (a -> Syntax.Source' a -> Syntax.Target' a -> Syntax.AsmInstr' a)
_instr_xor 64 = Syntax.XOR64
_instr_xor 32 = Syntax.XOR32
_instr_xor 16 = Syntax.XOR16
_instr_xchg :: Int -> (a -> Syntax.Source' a -> Syntax.Target' a -> Syntax.AsmInstr' a)
_instr_xchg 64 = Syntax.XCHG64
_instr_xchg 32 = Syntax.XCHG32
_instr_xchg 16 = Syntax.XCHG16
_instr_sal :: Int -> (a -> Syntax.Source' a -> Syntax.Target' a -> Syntax.AsmInstr' a)
_instr_sal 64 = Syntax.SAL64
_instr_sal 32 = Syntax.SAL32
_instr_sal 16 = Syntax.SAL16
_instr_sar :: Int -> (a -> Syntax.Source' a -> Syntax.Target' a -> Syntax.AsmInstr' a)
_instr_sar 64 = Syntax.SAR64
_instr_sar 32 = Syntax.SAR32
_instr_sar 16 = Syntax.SAR16

add :: (IsTarget t size t64 t32 t16 t8) => a -> t -> t -> ASMGenerator a ()
add pos arg1 arg2 = do
	_emitInstr pos [_wrap pos $ (_instr_add (sizeOf arg1)) pos (asSource pos arg1) (asTarget pos arg2)]


and :: (IsTarget t size t64 t32 t16 t8) => a -> t -> t -> ASMGenerator a ()
and pos arg1 arg2 = do
	_emitInstr pos [_wrap pos $ (_instr_and (sizeOf arg1)) pos (asSource pos arg1) (asTarget pos arg2)]


cmp :: (IsTarget t size t64 t32 t16 t8) => a -> t -> t -> ASMGenerator a ()
cmp pos arg1 arg2 = do
	_emitInstr pos [_wrap pos $ (_instr_cmp (sizeOf arg1)) pos (asSource pos arg1) (asTarget pos arg2)]


idiv :: (IsTarget t size t64 t32 t16 t8) => a -> t -> t -> ASMGenerator a ()
idiv pos arg1 arg2 = do
	_emitInstr pos [_wrap pos $ (_instr_idiv (sizeOf arg1)) pos (asSource pos arg1) (asTarget pos arg2)]


imul :: (IsTarget t size t64 t32 t16 t8) => a -> t -> t -> ASMGenerator a ()
imul pos arg1 arg2 = do
	_emitInstr pos [_wrap pos $ (_instr_imul (sizeOf arg1)) pos (asSource pos arg1) (asTarget pos arg2)]


lea :: (IsTarget t size t64 t32 t16 t8) => a -> t -> t -> ASMGenerator a ()
lea pos arg1 arg2 = do
	_emitInstr pos [_wrap pos $ (_instr_lea (sizeOf arg1)) pos (asSource pos arg1) (asTarget pos arg2)]


mov :: (IsTarget t size t64 t32 t16 t8) => a -> t -> t -> ASMGenerator a ()
mov pos arg1 arg2 = do
	_emitInstr pos [_wrap pos $ (_instr_mov (sizeOf arg1)) pos (asSource pos arg1) (asTarget pos arg2)]


sub :: (IsTarget t size t64 t32 t16 t8) => a -> t -> t -> ASMGenerator a ()
sub pos arg1 arg2 = do
	_emitInstr pos [_wrap pos $ (_instr_sub (sizeOf arg1)) pos (asSource pos arg1) (asTarget pos arg2)]


test :: (IsTarget t size t64 t32 t16 t8) => a -> t -> t -> ASMGenerator a ()
test pos arg1 arg2 = do
	_emitInstr pos [_wrap pos $ (_instr_test (sizeOf arg1)) pos (asSource pos arg1) (asTarget pos arg2)]


xor :: (IsTarget t size t64 t32 t16 t8) => a -> t -> t -> ASMGenerator a ()
xor pos arg1 arg2 = do
	_emitInstr pos [_wrap pos $ (_instr_xor (sizeOf arg1)) pos (asSource pos arg1) (asTarget pos arg2)]


xchg :: (IsTarget t size t64 t32 t16 t8) => a -> t -> t -> ASMGenerator a ()
xchg pos arg1 arg2 = do
	_emitInstr pos [_wrap pos $ (_instr_xchg (sizeOf arg1)) pos (asSource pos arg1) (asTarget pos arg2)]


sal :: (IsTarget t size t64 t32 t16 t8) => a -> t -> t -> ASMGenerator a ()
sal pos arg1 arg2 = do
	_emitInstr pos [_wrap pos $ (_instr_sal (sizeOf arg1)) pos (asSource pos arg1) (asTarget pos arg2)]


sar :: (IsTarget t size t64 t32 t16 t8) => a -> t -> t -> ASMGenerator a ()
sar pos arg1 arg2 = do
	_emitInstr pos [_wrap pos $ (_instr_sar (sizeOf arg1)) pos (asSource pos arg1) (asTarget pos arg2)]

