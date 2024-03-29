-- Instruction set used by the codegen and functions to emit them.
-- Uses AT&T syntax.
module IR.CodeGen.Emit (
    EmitM(..),
    emitAsString,
    add,
    and,
    callAddress,
    callDirect,
    cdq,
    cmp,
    commentMultiline,
    constDef,
    decrStack,
    extern,
    globalMain,
    idiv,
    imul,
    incrStack,
    jmp,
    jz,
    label,
    lea,
    leaOfConst,
    leave,
    movConst,
    mov,
    neg,
    pop,
    push,
    quadDef,
    longDef,
    ret,
    sal,
    sar,
    sete,
    setg,
    setge,
    setl,
    setle,
    setne,
    sub,
    test,
    xchg,
    xor,
    global,
) where

import qualified Backend.X64.Parser.Constructor as X64
import           Data.Int
import           IR.Syntax.Syntax
import           Prelude               hiding (and)
import           IR.CodeGen.Consts
import           IR.Size

class EmitM m where
    -- Emit a single instruction.
    emit :: String -> m ()

newtype PhonyEmit a = PE String

instance EmitM PhonyEmit where
    emit = PE

emitAsString :: PhonyEmit () -> String
emitAsString f = let PE s = f in s

-- Emit an addition operation between a source and destination location.
-- Saves the result in the destination.
--   add<s> <src>, <dest> # <comment>
-- where <s> is the AT&T instruction suffix based on <size>.
add :: EmitM m => X64.Loc -> X64.Loc -> String -> m ()
add src dest comment_ =
    let srcString = loc X64.Size32 src
        destString = loc X64.Size32 dest
    in emitInd $ bin "add" X64.Size32 srcString destString comment_

-- Emit an and operation between a source and destination location.
-- Saves the result in the destination.
--   and<s> <src>, <dest>
-- where <s> is the AT&T instruction suffix based on <size>.
and :: EmitM m => X64.Size -> X64.Loc -> X64.Loc -> String -> m ()
and size src dest comment_ =
    let srcString = loc size src
        destString = loc size dest
    in emitInd $ bin "and" size srcString destString comment_

-- Emit a call instruction to the address in the given memory location.
--   call *<offset>(%<reg>) # <comment>
callAddress :: EmitM m => X64.Reg -> Int64 -> String -> m ()
callAddress reg_ offset comment_ =
    emitInd $ "call *" ++ show offset ++ "(" ++ sizedReg X64.Size64 reg_ ++ ")" ++ comment comment_

-- Emit a call instruction to a label.
--   call <f>
callDirect :: EmitM m => String -> m ()
callDirect f = emitInd $ "call " ++ sanitiseAssembly f

-- Emit a sign-extend instruction for division purposes, see idiv.
-- Loads the sign of eax into edx.
--   cdq
cdq :: EmitM m => m ()
cdq = emitInd "cdq"

-- Emit a comparison between two locations, where the first location
-- is logically the right-hand-side of the comparison.
--   cmp<s> <src>, <dest>
-- where <s> is the AT&T instruction suffix based on <size>.
cmp :: EmitM m => X64.Size -> X64.Loc -> X64.Loc -> m ()
cmp size rhs lhs =
    let rhsString = loc size rhs
        lhsString = loc size lhs
    in emitInd $ bin "cmp" size rhsString lhsString ""

-- Emit a label to a compile-time constant string.
constDef :: EmitM m => Const -> m ()
constDef c = emit $ constName c ++ ":\n  .string " ++ show (constValue c)

-- Emit an instruction logically decreasing the stack
-- by increasing the rsp pointer.
--   subq $<n>, %rsp # <comment>
decrStack :: EmitM m => Int64 -> m ()
decrStack n = emitInd $ "addq " ++ lit64 n ++ ", %rsp"

-- Emit a declaration of an external call target.
extern :: EmitM m => String -> m ()
extern s = emit $ ".extern " ++ s

-- Emit a declaration of the main function as entry point.
globalMain :: EmitM m => m ()
globalMain = emit ".global main"

global :: EmitM m => IRLabelName -> m ()
global (IRLabelName l) = emit $ ".global " ++ sanitiseAssembly l

-- Emit an instruction logically increasing the stack
-- by decreasing the rsp pointer.
--   subq $<n>, %rsp # <comment>
incrStack :: EmitM m => Int64 -> String -> m ()
incrStack n comment_ = emitInd $ "subq " ++ lit64 n ++ ", %rsp" ++ comment comment_

-- Emit a division instruction.
-- The left-hand-side has to be loaded to the eax register
-- and sign-extended to edx manually or by using the cdq instruction.
-- The other location must not be an immediate.
-- The division is stored in eax and the remainder in edx.
--   idiv<s> <loc>
-- where <s> is the AT&T instruction suffix based on <size>.
idiv :: EmitM m => X64.Size -> X64.Loc -> m ()
idiv size loc_ = case loc_ of
    X64.LocConst {} -> error "internal error. idiv on an immediate."
    X64.LocMem {} -> error "internal error. idiv with ptr"
    _         -> emitInd $ "idiv" ++ sizeSuf size ++ " " ++ loc size loc_

-- Emit a multiplication operation between a source and destination location.
--   imul<s> <src>, <dest>
-- where <s> is the AT&T instruction suffix based on <size>.
imul :: EmitM m => X64.Loc -> X64.Loc -> m ()
imul src dest =
    let srcString = loc X64.Size32 src
        destString = loc X64.Size32 dest
    in emitInd $ bin "imul" X64.Size32 srcString destString ""

-- Emit a jump to a label.
--   jmp <l>
jmp :: EmitM m => IRLabelName -> m ()
jmp (IRLabelName l) = emitInd $ "jmp " ++ sanitiseAssembly l

-- Emit a conditional jump instruction that tests the ZF CPU flag
-- and jumps if it is set.
--   jz <l>
jz :: EmitM m => IRLabelName -> m ()
jz (IRLabelName l) = emitInd $ "jz " ++ sanitiseAssembly l

-- Emit a label.
-- <l>: # <comment>
label :: EmitM m => IRLabelName -> String -> m ()
label (IRLabelName l) comment_ = emit $ sanitiseAssembly l ++ ":" ++ comment comment_

lea :: EmitM m => X64.Size -> X64.Loc -> X64.Loc -> String -> m ()
lea size src dest comment_ =
    emitInd $ bin "lea" size (loc size src) (loc size dest) comment_

-- Emit an address load operation for a compile-time constant.
--   lea <x>(%rip), %<reg>
-- where <s> is the AT&T instruction suffix based on <size>.
leaOfConst :: EmitM m => String -> X64.Reg -> m ()
leaOfConst x dest =
    emitInd $ "lea " ++ sanitiseAssembly x ++ "(%rip), " ++ reg (X64.showReg X64.Size64 dest)

-- Emit a standard epilogue leave instruction
-- that restores the rsp and rbp registers.
leave :: EmitM m => m ()
leave = emitInd "leave"

-- Move the pointer to a constant into a location.
--   movq $<i>, <loc>
movConst :: EmitM m => IRLabelName -> X64.Reg -> m ()
movConst (IRLabelName i) reg_ = emitInd $ bin "mov" X64.Size64 (i ++ "(%rip)") (sizedReg X64.Size64 reg_) ""

mov :: EmitM m => X64.Size -> X64.Loc -> X64.Loc -> String -> m ()
mov size src dest comment_ = emitInd $ bin "mov" size (loc size src) (loc size dest) comment_

{-
-- Emit a move from a source location to a register.
--   mov<s> <src>, %<dest> # <comment>
-- where <s> is the AT&T instruction suffix based on <size>.
movToReg :: EmitM m => X64.Size -> X64.Loc -> Reg -> String -> m ()
movToReg size src dest comment_ =
    let srcString = loc size src
    in  emitInd $ bin "mov" size srcString (sizedReg size dest) comment_

-- Emit a move from a source location to a stack destination.
--   mov<s> <src>, <stackDest>(%rbp) # <comment>
-- where <s> is the AT&T instruction suffix based on <size>.
movToStack :: EmitM m => X64.Size -> X64.Loc -> Int64 -> String -> m ()
movToStack size src stackDest comment_ = case src of
    X64.LocReg reg_   -> emitInd $ bin "mov" size (sizedReg size reg_) (stack stackDest) comment_
    X64.LocPtr src' _ -> movToStack size src' stackDest comment_
    X64.LocImm int    -> emitInd $ bin "mov" size (lit32 int) (stack stackDest) comment_
    X64.LocImm64 int  -> emitInd $ bin "mov" size (lit64 int) (stack stackDest) comment_
    X64.LocStack _    -> error "internal error. mov from stack to stack"

-- Emit a move from a memory location pointed to by the value in the `ptrReg`
-- offset by `ptrOffset` bytes to the destination register.
--  mov<s> <ptrOffset>(%<ptrReg>), %<destReg> # <comment>
-- where <s> is the AT&T instruction suffix based on <size>.
movFromMemToReg :: EmitM m => X64.Size -> Reg -> Int64 -> Reg -> String -> m ()
movFromMemToReg size ptrReg ptrOffset destReg comment_ =
    emitInd $ bin "mov" size (ptr X64.Size64 ptrReg ptrOffset) (sizedReg size destReg) comment_

-- Emit a move from a register to the memory location pointed to by the
-- value in the `ptrReg` offset by `ptrOffset` bytes.
--   mov<s> %<srcReg>, <ptrOffset>(%<ptrReg>)
-- where <s> is the AT&T instruction suffix based on <size>.
movFromRegToMem :: EmitM m => X64.Size -> Reg -> Reg -> Int64 -> m ()
movFromRegToMem size srcReg ptrReg ptrOffset =
    emitInd $ bin "mov" size (sizedReg size srcReg) (ptr X64.Size64 ptrReg ptrOffset) ""
-}

-- Emit a negation operation on a register.
--   neg %<reg>
neg :: EmitM m => X64.Loc -> m ()
neg l = emitInd $ "neg" ++ sizeSuf X64.Size32 ++ " " ++ loc X64.Size32 l

-- Emit a pop instruction that pops the top of the stack into the location.
-- The size of the pop is always 8 bytes.
--   pop <loc>
pop :: EmitM m => X64.Loc -> m ()
pop srcloc = emitInd $ "pop " ++ loc X64.Size64 srcloc

-- Emit a push instruction that pushes contents of the location on the stack.
-- The size of the push is always 8 bytes.
--   push <loc>
push :: EmitM m => X64.Loc -> String -> m ()
push srcloc comment_ = emitInd $ "push " ++ loc X64.Size64 srcloc ++ comment comment_

-- Emit a definition of a quad value.
--   .quad <s>
quadDef :: EmitM m => String -> m ()
quadDef s = emitInd $ ".quad " ++ sanitiseAssembly s

-- Emit a definition of a long value.
--   .long <s>
longDef :: EmitM m => String -> m ()
longDef s = emitInd $ ".long " ++ sanitiseAssembly s

-- Emit a ret instruction that ends the current function call.
ret :: EmitM m => m ()
ret = emitInd "ret"

-- Emit an instruction that shifts a register bitwise to the left by a given offset.
-- Logically this is a multiply-by-2^n operation.
--   sall $<n>, %<reg>
sal :: EmitM m => Int -> X64.Loc -> String -> m ()
sal n loc_ comment_ = emitInd $ "sal " ++ lit n ++ ", " ++ loc X64.Size32 loc_ ++ comment comment_

-- Emit an instruction that shifts a register bitwise to the right by a given offset.
-- Logically this is a divide-by-2^n operation.
--   sarl $<n>, %<reg>
sar :: EmitM m => Int -> X64.Loc -> String -> m ()
sar n loc_ comment_ = emitInd $ "sar " ++ lit n ++ ", " ++ loc X64.Size32 loc_ ++ comment comment_

-- Emit an instruction that loads 1 or 0 into a register based on
-- the result of the previous cmp operation interpreted as an
-- equal-to comparison.
--   sete %<reg>
sete :: EmitM m => X64.Loc -> m ()
sete loc_ = emitInd $ "sete " ++ loc X64.Size8 loc_

-- Emit an instruction that loads 1 or 0 into a register based on
-- the result of the previous cmp operation interpreted as a
-- greater-than comparison.
--   setg %<reg>
setg :: EmitM m => X64.Loc -> m ()
setg loc_ = emitInd $ "setg " ++ loc X64.Size8 loc_

-- Emit an instruction that loads 1 or 0 into a register based on
-- the result of the previous cmp operation interpreted as a
-- greater-than-or-equal-to comparison.
--   setge %<reg>
setge :: EmitM m => X64.Loc -> m ()
setge loc_ = emitInd $ "setge " ++ loc X64.Size8 loc_

-- Emit an instruction that loads 1 or 0 into a register based on
-- the result of the previous cmp operation interpreted as a
-- less-than comparison.
--   setl %<reg>
setl :: EmitM m => X64.Loc -> m ()
setl loc_ = emitInd $ "setl " ++ loc X64.Size8 loc_

-- Emit an instruction that loads 1 or 0 into a register based on
-- the result of the previous cmp operation interpreted as a
-- less-than-or-equal-to comparison.
--   setle %<reg>
setle :: EmitM m => X64.Loc -> m ()
setle loc_ = emitInd $ "setle " ++ loc X64.Size8 loc_

-- Emit an instruction that loads 1 or 0 into a register based on
-- the result of the previous cmp operation interpreted as a
-- not-equal-to comparison.
--   setne %<reg>
setne :: EmitM m => X64.Loc -> m ()
setne loc_ = emitInd $ "setne " ++ loc X64.Size8 loc_

-- Emit a subtraction operation between a source and destination location (dest - src).
--   sub<s> <src>, <dest>
-- where <s> is the AT&T instruction suffix based on <size>.
sub :: EmitM m => X64.Loc -> X64.Loc -> m ()
sub src dest =
    let srcString = loc X64.Size32 src
        destString = loc X64.Size32 dest
    in emitInd $ bin "sub" X64.Size32 srcString destString ""

-- Emit a test instruction between two locations that sets CPU flags
-- based on the result of a bitwise-and performed on the operands.
-- The operands are considered for their lower 8 bytes only.
--   testb <op1>, <op2>
test :: EmitM m => X64.Size -> X64.Loc -> X64.Loc -> m ()
test size op1 op2 =
    let op1String = loc size op1
        op2String = loc size op2
    in emitInd $ bin "test" size op1String op2String ""

xchg :: EmitM m => X64.Size -> X64.Loc -> X64.Loc -> m ()
xchg size src dest =
    let srcString = loc size src
        destString = loc size dest
    in emitInd $ bin "xchg" size srcString destString ""

-- Emit a xor operation between a source and destination location.
-- Saves the result in the destination.
--   xor<s> <src>, <dest>
-- where <s> is the AT&T instruction suffix based on <size>.
xor :: EmitM m => X64.Size -> X64.Loc -> X64.Loc -> m ()
xor size src dest =
    let srcString = loc size src
        destString = loc size dest
    in emitInd $ bin "xor" size srcString destString ""

-- String representation of a binary instruction with a given size suffix,
-- two operands and an end-of-line comment.
bin :: String -> X64.Size -> String -> String -> String -> String
bin instr size x y comment_ = instr ++ sizeSuf size ++ " " ++ x ++ ", " ++ y ++ comment comment_

-- String representation of an end-of-line comment
comment :: String -> String
comment [] = []
comment s  = ' ':'#':' ':s

commentMultiline :: EmitM m => [String] -> m ()
commentMultiline = emit . unlines . map comment

complexPtr :: X64.Size -> X64.Reg -> Int64 -> X64.Reg -> X64.Size -> String
complexPtr size baseLoc offset idxLoc scale =
    show offset ++ "("
                ++ sizedReg size baseLoc ++ ","
                ++ sizedReg size idxLoc ++ ","
                ++ show (X64.toBytes scale) ++ ")"

-- Emit an instruction indented by 2 spaces.
emitInd :: EmitM m => String -> m ()
emitInd s = emit ("  " ++ s)

-- String representation of an integral literal.
lit :: Int -> String
lit n = '$':show n

-- String representation of a memory access,
-- where the base is located in the given register and is offset
-- by the passed offset.
ptr :: X64.Size -> X64.Reg -> Int64 -> String
ptr size r offset = show offset ++ "(" ++ sizedReg size r ++ ")"

-- String representation of an integral literal.
lit32 :: Int32 -> String
lit32 n = '$':show n

-- String representation of an integral literal.
lit64 :: Int64 -> String
lit64 n = '$':show n

-- String representation of a location.
loc :: X64.Size -> X64.Loc -> String
loc size loc_ = case loc_ of
    X64.LocReg r                          -> sizedReg size r
    X64.LocMem (r, n)                        -> ptr X64.Size64 r n
    X64.LocMemOffset base idx offset scale -> complexPtr X64.Size64 base offset idx scale
    X64.LocConst n                          -> lit64 $ fromIntegral n
    X64.LocLabel label                    -> label

-- String representation of a register (full 64-bits).
reg :: String -> String
reg r = '%':r

-- String representation of a register identifier for a given size.
sizedReg :: X64.Size -> X64.Reg -> String
sizedReg size r = case size of
    X64.Size8      -> reg $ X64.showReg X64.Size8 r
    X64.Size32    -> reg $ X64.showReg X64.Size32 r
    X64.Size64 -> reg $ X64.showReg X64.Size64 r

-- AT&T size suffix for a given operand size.
sizeSuf :: X64.Size -> String
sizeSuf s = case s of
    X64.Size8      -> "b"
    X64.Size32    -> "l"
    X64.Size64 -> "q"
