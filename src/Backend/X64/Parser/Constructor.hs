
module Backend.X64.Parser.Constructor where

import qualified Backend.X64.Parser.AbsXGAS as Syntax

# Registers

data Reg64 = RAX| RBX| RCX| RDX| RDI| RSI| RSP| RBP| R8| R9| R10| R11| R12| R13| R14| R15
deriving (Eq, Ord, Show, Read, Generic)




data Reg32 = EAX| EBX| ECX| EDX| EDI| ESI| ESP| EBP| R8D| R9D| R10D| R11D| R12D| R13D| R14D| R15D
deriving (Eq, Ord, Show, Read, Generic)




data Reg16 = AX| BX| CX| DX| DI| SI| SP| BP| R8W| R9W| R10W| R11W| R12W| R13W| R14W| R15W
deriving (Eq, Ord, Show, Read, Generic)




data Reg8 = AL| BL| CL| DL| DIL| SIL| SPL| BPL| R8B| R9B| R10B| R11B| R12B| R13B| R14B| R15B
deriving (Eq, Ord, Show, Read, Generic)




# Size classes

class IsTarget64 target target32 target16 target8 | target -> target32 target16 target8 where
to32 :: target -> target32
to16 :: target -> target16
to8 :: target -> target8

to64 :: target -> target
default to64 :: target -> target
to64 = id

asSource :: a -> target -> Syntax.Source' a
asTarget :: a -> target -> Syntax.Target' a
instance IsTarget64 Reg64 Reg32 Reg16 Reg8 where
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


class IsTarget32 target target64 target16 target8 | target -> target64 target16 target8 where
to64 :: target -> target64
to16 :: target -> target16
to8 :: target -> target8

to32 :: target -> target
default to32 :: target -> target
to32 = id

asSource :: a -> target -> Syntax.Source' a
asTarget :: a -> target -> Syntax.Target' a
instance IsTarget32 Reg32 Reg64 Reg16 Reg8 where
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


class IsTarget16 target target64 target32 target8 | target -> target64 target32 target8 where
to64 :: target -> target64
to32 :: target -> target32
to8 :: target -> target8

to16 :: target -> target
default to16 :: target -> target
to16 = id

asSource :: a -> target -> Syntax.Source' a
asTarget :: a -> target -> Syntax.Target' a
instance IsTarget16 Reg16 Reg64 Reg32 Reg8 where
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


class IsTarget8 target target64 target32 target16 | target -> target64 target32 target16 where
to64 :: target -> target64
to32 :: target -> target32
to16 :: target -> target16

to8 :: target -> target
default to8 :: target -> target
to8 = id

asSource :: a -> target -> Syntax.Source' a
asTarget :: a -> target -> Syntax.Target' a
instance IsTarget8 Reg8 Reg64 Reg32 Reg16 where
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


# Instruction wrappers

add64 :: (IsTarget64 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
add64 pos arg1 arg2 = Syntax.ADD64 pos (asSource pos arg1) (asTarget pos arg2)


add32 :: (IsTarget32 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
add32 pos arg1 arg2 = Syntax.ADD32 pos (asSource pos arg1) (asTarget pos arg2)


add16 :: (IsTarget16 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
add16 pos arg1 arg2 = Syntax.ADD16 pos (asSource pos arg1) (asTarget pos arg2)


add8 :: (IsTarget8 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
add8 pos arg1 arg2 = Syntax.ADD8 pos (asSource pos arg1) (asTarget pos arg2)


and64 :: (IsTarget64 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
and64 pos arg1 arg2 = Syntax.AND64 pos (asSource pos arg1) (asTarget pos arg2)


and32 :: (IsTarget32 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
and32 pos arg1 arg2 = Syntax.AND32 pos (asSource pos arg1) (asTarget pos arg2)


and16 :: (IsTarget16 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
and16 pos arg1 arg2 = Syntax.AND16 pos (asSource pos arg1) (asTarget pos arg2)


and8 :: (IsTarget8 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
and8 pos arg1 arg2 = Syntax.AND8 pos (asSource pos arg1) (asTarget pos arg2)


cmp64 :: (IsTarget64 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
cmp64 pos arg1 arg2 = Syntax.CMP64 pos (asSource pos arg1) (asTarget pos arg2)


cmp32 :: (IsTarget32 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
cmp32 pos arg1 arg2 = Syntax.CMP32 pos (asSource pos arg1) (asTarget pos arg2)


cmp16 :: (IsTarget16 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
cmp16 pos arg1 arg2 = Syntax.CMP16 pos (asSource pos arg1) (asTarget pos arg2)


cmp8 :: (IsTarget8 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
cmp8 pos arg1 arg2 = Syntax.CMP8 pos (asSource pos arg1) (asTarget pos arg2)


idiv64 :: (IsTarget64 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
idiv64 pos arg1 arg2 = Syntax.IDIV64 pos (asSource pos arg1) (asTarget pos arg2)


idiv32 :: (IsTarget32 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
idiv32 pos arg1 arg2 = Syntax.IDIV32 pos (asSource pos arg1) (asTarget pos arg2)


idiv16 :: (IsTarget16 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
idiv16 pos arg1 arg2 = Syntax.IDIV16 pos (asSource pos arg1) (asTarget pos arg2)


idiv8 :: (IsTarget8 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
idiv8 pos arg1 arg2 = Syntax.IDIV8 pos (asSource pos arg1) (asTarget pos arg2)


imul64 :: (IsTarget64 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
imul64 pos arg1 arg2 = Syntax.IMUL64 pos (asSource pos arg1) (asTarget pos arg2)


imul32 :: (IsTarget32 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
imul32 pos arg1 arg2 = Syntax.IMUL32 pos (asSource pos arg1) (asTarget pos arg2)


imul16 :: (IsTarget16 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
imul16 pos arg1 arg2 = Syntax.IMUL16 pos (asSource pos arg1) (asTarget pos arg2)


imul8 :: (IsTarget8 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
imul8 pos arg1 arg2 = Syntax.IMUL8 pos (asSource pos arg1) (asTarget pos arg2)


lea64 :: (IsTarget64 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
lea64 pos arg1 arg2 = Syntax.LEA64 pos (asSource pos arg1) (asTarget pos arg2)


lea32 :: (IsTarget32 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
lea32 pos arg1 arg2 = Syntax.LEA32 pos (asSource pos arg1) (asTarget pos arg2)


lea16 :: (IsTarget16 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
lea16 pos arg1 arg2 = Syntax.LEA16 pos (asSource pos arg1) (asTarget pos arg2)


lea8 :: (IsTarget8 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
lea8 pos arg1 arg2 = Syntax.LEA8 pos (asSource pos arg1) (asTarget pos arg2)


mov64 :: (IsTarget64 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
mov64 pos arg1 arg2 = Syntax.MOV64 pos (asSource pos arg1) (asTarget pos arg2)


mov32 :: (IsTarget32 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
mov32 pos arg1 arg2 = Syntax.MOV32 pos (asSource pos arg1) (asTarget pos arg2)


mov16 :: (IsTarget16 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
mov16 pos arg1 arg2 = Syntax.MOV16 pos (asSource pos arg1) (asTarget pos arg2)


mov8 :: (IsTarget8 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
mov8 pos arg1 arg2 = Syntax.MOV8 pos (asSource pos arg1) (asTarget pos arg2)


sub64 :: (IsTarget64 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
sub64 pos arg1 arg2 = Syntax.SUB64 pos (asSource pos arg1) (asTarget pos arg2)


sub32 :: (IsTarget32 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
sub32 pos arg1 arg2 = Syntax.SUB32 pos (asSource pos arg1) (asTarget pos arg2)


sub16 :: (IsTarget16 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
sub16 pos arg1 arg2 = Syntax.SUB16 pos (asSource pos arg1) (asTarget pos arg2)


sub8 :: (IsTarget8 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
sub8 pos arg1 arg2 = Syntax.SUB8 pos (asSource pos arg1) (asTarget pos arg2)


test64 :: (IsTarget64 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
test64 pos arg1 arg2 = Syntax.TEST64 pos (asSource pos arg1) (asTarget pos arg2)


test32 :: (IsTarget32 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
test32 pos arg1 arg2 = Syntax.TEST32 pos (asSource pos arg1) (asTarget pos arg2)


test16 :: (IsTarget16 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
test16 pos arg1 arg2 = Syntax.TEST16 pos (asSource pos arg1) (asTarget pos arg2)


test8 :: (IsTarget8 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
test8 pos arg1 arg2 = Syntax.TEST8 pos (asSource pos arg1) (asTarget pos arg2)


xor64 :: (IsTarget64 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
xor64 pos arg1 arg2 = Syntax.XOR64 pos (asSource pos arg1) (asTarget pos arg2)


xor32 :: (IsTarget32 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
xor32 pos arg1 arg2 = Syntax.XOR32 pos (asSource pos arg1) (asTarget pos arg2)


xor16 :: (IsTarget16 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
xor16 pos arg1 arg2 = Syntax.XOR16 pos (asSource pos arg1) (asTarget pos arg2)


xor8 :: (IsTarget8 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
xor8 pos arg1 arg2 = Syntax.XOR8 pos (asSource pos arg1) (asTarget pos arg2)


xchg64 :: (IsTarget64 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
xchg64 pos arg1 arg2 = Syntax.XCHG64 pos (asSource pos arg1) (asTarget pos arg2)


xchg32 :: (IsTarget32 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
xchg32 pos arg1 arg2 = Syntax.XCHG32 pos (asSource pos arg1) (asTarget pos arg2)


xchg16 :: (IsTarget16 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
xchg16 pos arg1 arg2 = Syntax.XCHG16 pos (asSource pos arg1) (asTarget pos arg2)


xchg8 :: (IsTarget8 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
xchg8 pos arg1 arg2 = Syntax.XCHG8 pos (asSource pos arg1) (asTarget pos arg2)


sal64 :: (IsTarget64 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
sal64 pos arg1 arg2 = Syntax.SAL64 pos (asSource pos arg1) (asTarget pos arg2)


sal32 :: (IsTarget32 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
sal32 pos arg1 arg2 = Syntax.SAL32 pos (asSource pos arg1) (asTarget pos arg2)


sal16 :: (IsTarget16 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
sal16 pos arg1 arg2 = Syntax.SAL16 pos (asSource pos arg1) (asTarget pos arg2)


sal8 :: (IsTarget8 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
sal8 pos arg1 arg2 = Syntax.SAL8 pos (asSource pos arg1) (asTarget pos arg2)


sar64 :: (IsTarget64 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
sar64 pos arg1 arg2 = Syntax.SAR64 pos (asSource pos arg1) (asTarget pos arg2)


sar32 :: (IsTarget32 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
sar32 pos arg1 arg2 = Syntax.SAR32 pos (asSource pos arg1) (asTarget pos arg2)


sar16 :: (IsTarget16 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
sar16 pos arg1 arg2 = Syntax.SAR16 pos (asSource pos arg1) (asTarget pos arg2)


sar8 :: (IsTarget8 t t1 t2 t3) => a -> t -> t -> AsmInstr' a
sar8 pos arg1 arg2 = Syntax.SAR8 pos (asSource pos arg1) (asTarget pos arg2)

