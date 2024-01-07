{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
module Backend.X86.Syntax where

import Data.Generics.Product
import Data.Generics.Sum
import GHC.Generics (Generic, Generic1)
import Control.DeepSeq
import Control.Lens hiding(Empty, Index, Const)

import qualified Linearized.Syntax as IR

class (HasPosition 1 (s IR.IRPosition) (s IR.IRPosition) IR.IRPosition IR.IRPosition
    , Show (s IR.IRPosition)
    ) => IsASM (s :: * -> *)

data Program a = Program a [Instruction a]
    deriving (Eq, Ord, Read, Generic, Foldable, Traversable, Functor, NFData)
instance IsASM Program

data Value a = Constant a Integer 
           | Label a String 
           | Register a (Reg a) 
           | Memory a (Reg a)  (Maybe ((Reg a), Integer)) (Maybe Integer) (Maybe (IR.Type a))
            --[r1 + r2*c1 + c2]
            | Local a Integer
    deriving (Eq, Ord, Read, Generic, Foldable, Traversable, Functor, NFData)
instance IsASM Value

data Instruction a = ADD a (Value a) (Value a)
                 | SUB a (Value a) (Value a)
                 | IMUL a (Value a) (Value a)
                 | CDQ a
                 | IDIV a (Value a)
                 | MOV a (Value a) (Value a)
                 | AND a (Value a) (Value a)
                 | OR a (Value a) (Value a)
                 | XOR a (Value a) (Value a)
                 | INC a (Value a)
                 | SETZ a (Value a)
                 | JMP a (Value a)
                 | JZ a (Value a)
                 | JNZ a (Value a)
                 | JE a (Value a)
                 | JNE a (Value a)
                 | JL a (Value a)
                 | JLE a (Value a)
                 | JG a (Value a)
                 | JGE a (Value a)
                 | CMP a (Value a) (Value a)
                 | TEST a (Value a) (Value a)
                 | PUSH a (Value a)
                 | POP a (Value a)
                 | CALL a (Value a)
                 | LEAVE a
                 | RET a
                 | SetLabel a String
                 | Section a String
                 | Global a String
                 | DB a (Value a)
                 | DW a (Value a)
                 | DD a (Value a)
                 | DQ a (Value a)
    deriving (Eq, Show, Ord, Read, Generic, Foldable, Traversable, Functor, NFData)
instance IsASM Instruction

data Reg a = R11 a | R11D a | R11B a 
         | R10 a | R10D a | R10B a 
         | R9 a  | R9D a | R9B a 
         | R8 a  | R8D a | R8B a 
         | RDX a | EDX a | DL a
         | RCX a | ECX a | CL a
         | RAX a | EAX a | AL a
         | RBP a
         | RSP a
         | RSI a | ESI a | SIL a
         | RDI a | EDI a | DIL a
         | RBX a | EBX a | BL a  --callee saved
         | R12 a | R12D a | R12B a --callee saved
         | R13 a | R13D a | R13B a --callee saved
         | R14 a | R14D a | R14B a --callee saved
         | R15 a | R15D a | R15B a --callee saved
    deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor, NFData)
instance IsASM Reg

instance Show (Value a) where
    show (Constant _ i) = show i
    show (Label _ s) = s
    show (Register _ r) = show r
    show (Local _ i) = "$+"++show i
    show (Memory _ r mm mo _) =
        let m = case mm of {Nothing -> ""; Just (r,i) -> " + "++show r ++ "*"++show i}
            o = case mo of 
                    Nothing -> ""
                    Just i -> 
                        if i > 0 then " + "++show i
                        else if i < 0 then " - "++show (-i)
                        else ""
        in "["++show r++m++o++"]"

instance Show (Program a) where
    show (Program _ is) = "%include 'lib/runtime.ext'\n" ++ (concat $ map p is)
        where
            p i = pp i ++ "\n"
            pp (SetLabel _ l) = l++":"
            pp (Section _ s) = "section ."++s
            pp (Global _ s) = "global "++s
            pp x = "    "++ppp x
            ppp (ADD _ v1 v2) = "ADD "++show v1++", "++show v2
            ppp (SUB _ v1 v2) = "SUB "++show v1++", "++show v2
            ppp (IMUL _ v1 v2) = "IMUL "++show v1++", "++show v2
            ppp (MOV _ v1 v2) = "MOV "++show v1++", "++show v2
            ppp (AND _ v1 v2) = "AND "++show v1++", "++show v2
            ppp (OR _ v1 v2) = "OR "++show v1++", "++show v2
            ppp (XOR _ v1 v2) = "XOR "++show v1++", "++show v2
            ppp (CMP _ v1 v2) = "CMP "++show v1++", "++show v2
            ppp (TEST _ v1 v2) = "TEST "++show v1++", "++show v2
            ppp i = show i

isReg (Register _ _) = True
isReg _ = False

isMem (Memory _ _ _ _ _) = True
isMem _ = False

topReg (AL pos) = RAX pos
topReg (EAX pos) = RAX pos
topReg (RAX pos) = RAX pos
topReg (BL pos) = RBX pos
topReg (EBX pos) = RBX pos
topReg (RBX pos) = RBX pos
topReg (CL pos) = RCX pos
topReg (ECX pos) = RCX pos
topReg (RCX pos) = RCX pos
topReg (DL pos) = RDX pos
topReg (EDX pos) = RDX pos
topReg (RDX pos) = RDX pos
topReg (EDI pos) = RDI pos
topReg (RDI pos) = RDI pos
topReg (DIL pos) = RDI pos
topReg (ESI pos) = RSI pos
topReg (RSI pos) = RSI pos
topReg (SIL pos) = RSI pos
topReg (R8B pos) = R8 pos
topReg (R8D pos) = R8 pos
topReg (R8 pos) = R8 pos
topReg (R9B pos) = R9 pos
topReg (R9D pos) = R9 pos
topReg (R9 pos) = R9 pos
topReg (R10B pos) = R10 pos
topReg (R10D pos) = R10 pos
topReg (R10 pos) = R10 pos
topReg (R11B pos) = R11 pos
topReg (R11D pos) = R11 pos
topReg (R11 pos) = R11 pos
topReg (R12B pos) = R12 pos
topReg (R12D pos) = R12 pos
topReg (R12 pos) = R12 pos
topReg (R13B pos) = R13 pos
topReg (R13D pos) = R13 pos
topReg (R13 pos) = R13 pos
topReg (R14B pos) = R14 pos
topReg (R14D pos) = R14 pos
topReg (R14 pos) = R14 pos
topReg (R15B pos) = R15 pos
topReg (R15D pos) = R15 pos
topReg (R15 pos) = R15 pos
topReg (RSP pos) = RSP pos

regSize (IR.Reference _) x = x
regSize (IR.IntT _) (RAX pos) = EAX pos
regSize (IR.IntT _) (RBX pos) = EBX pos
regSize (IR.IntT _) (RCX pos) = ECX pos
regSize (IR.IntT _) (RDX pos) = EDX pos
regSize (IR.IntT _) (RSI pos) = ESI pos
regSize (IR.IntT _) (RDI pos) = EDI pos
regSize (IR.IntT _) (R8 pos) = R8D pos
regSize (IR.IntT _) (R9 pos) = R9D pos
regSize (IR.IntT _) (R10 pos) = R10D pos
regSize (IR.IntT _) (R11 pos) = R11D pos
regSize (IR.IntT _) (R12 pos) = R12D pos
regSize (IR.IntT _) (R13 pos) = R13D pos
regSize (IR.IntT _) (R14 pos) = R14D pos
regSize (IR.IntT _) (R15 pos) = R15D pos
regSize (IR.ByteT _) (RAX pos) = AL pos
regSize (IR.ByteT _) (RBX pos) = BL pos
regSize (IR.ByteT _) (RCX pos) = CL pos
regSize (IR.ByteT _) (RDX pos) = DL pos
regSize (IR.ByteT _) (RSI pos) = SIL pos
regSize (IR.ByteT _) (RDI pos) = DIL pos
regSize (IR.ByteT _) (R8 pos) = R8B pos
regSize (IR.ByteT _) (R9 pos) = R9B pos
regSize (IR.ByteT _) (R10 pos) = R10B pos
regSize (IR.ByteT _) (R11 pos) = R11B pos
regSize (IR.ByteT _) (R12 pos) = R12B pos
regSize (IR.ByteT _) (R13 pos) = R13B pos
regSize (IR.ByteT _) (R14 pos) = R14B pos
regSize (IR.ByteT _) (R15 pos) = R15B pos
regSize t r = regSize t (topReg r)

regSizeV t (Register p r) = Register p (regSize t r)
regSizeV _ v = v

topRegV (Register p r) = Register p (topReg r)
topRegV v = v

regSizeRV (Register _ r) = regSizeR r

regSizeR (EAX pos) = IR.IntT pos
regSizeR (EBX pos) = IR.IntT pos
regSizeR (ECX pos) = IR.IntT pos
regSizeR (EDX pos) = IR.IntT pos
regSizeR (ESI pos) = IR.IntT pos
regSizeR (EDI pos) = IR.IntT pos
regSizeR (R8D pos) = IR.IntT pos
regSizeR (R9D pos) = IR.IntT pos
regSizeR (R10D pos) = IR.IntT pos
regSizeR (R11D pos) = IR.IntT pos
regSizeR (R12D pos) = IR.IntT pos
regSizeR (R13D pos) = IR.IntT pos
regSizeR (R14D pos) = IR.IntT pos
regSizeR (R15D pos) = IR.IntT pos
regSizeR (AL pos) = IR.ByteT pos
regSizeR (BL pos) = IR.ByteT pos
regSizeR (CL pos) = IR.ByteT pos
regSizeR (DL pos) = IR.ByteT pos
regSizeR (SIL pos) = IR.ByteT pos
regSizeR (DIL pos) = IR.ByteT pos
regSizeR (R8B pos) = IR.ByteT pos
regSizeR (R9B pos) = IR.ByteT pos
regSizeR (R10B pos) = IR.ByteT pos
regSizeR (R11B pos) = IR.ByteT pos
regSizeR (R12B pos) = IR.ByteT pos
regSizeR (R13B pos) = IR.ByteT pos
regSizeR (R14B pos) = IR.ByteT pos
regSizeR (R15B pos) = IR.ByteT pos
regSizeR x = IR.Reference $ getPosIR x

getPosIR :: IsASM s => (s IR.IRPosition) -> IR.IRPosition
getPosIR ast = view (position @1) ast

setPosIR :: (IsASM s) => IR.IRPosition -> (s IR.IRPosition) -> (s IR.IRPosition)
setPosIR p ast = set (position @1) p ast
