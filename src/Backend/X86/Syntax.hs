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
import Reporting.Errors.Position

import qualified Linearized.Syntax as IR

type Position = IR.IRPosition

class (HasPosition 1 (s IR.IRPosition) (s IR.IRPosition) IR.IRPosition IR.IRPosition
    , Show (s IR.IRPosition)
    ) => IsASM (s :: * -> *)

data Program a = Program a [Instruction a]
    deriving (Read, Generic, Foldable, Traversable, Functor, NFData)
instance IsASM Program

data Value a = Constant a Integer 
           | Label a String 
           | Register a (Reg a) 
           | Memory a (Reg a)  (Maybe ((Reg a), Integer)) (Maybe Integer) (Maybe (IR.Type a))
            --[r1 + r2*c1 + c2]
            | Local a Integer
    deriving (Read, Generic, Foldable, Traversable, Functor, NFData)
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
    deriving (Read, Generic, Foldable, Traversable, Functor, NFData)
instance IsASM Instruction

renderStringComment :: IR.IRPosition -> String -> String
renderStringComment (IR.IRPosition _ (Undefined, _)) code = code
renderStringComment (IR.IRPosition _ (Position id file line col, _)) code = code ++ " ; " ++ show file ++ ": " ++ show line ++ "," ++ show col
renderStringComment (IR.IRPosition _ _) code = code

instance Show (Instruction IR.IRPosition) where
    show (ADD p v1 v2) = renderStringComment p $ "ADD " ++ show v1 ++ ", " ++ show v2
    show (SUB p v1 v2) = renderStringComment p $ "SUB " ++ show v1 ++ ", " ++ show v2
    show (IMUL p v1 v2) = renderStringComment p $ "IMUL " ++ show v1 ++ ", " ++ show v2
    show (CDQ p) = renderStringComment p $ "CDQ"
    show (IDIV p v) = renderStringComment p $ "IDIV " ++ show v
    show (MOV p v1 v2) = renderStringComment p $ "MOV " ++ show v1 ++ ", " ++ show v2
    show (AND p v1 v2) = renderStringComment p $ "AND " ++ show v1 ++ ", " ++ show v2
    show (OR p v1 v2) = renderStringComment p $ "OR " ++ show v1 ++ ", " ++ show v2
    show (XOR p v1 v2) = renderStringComment p $ "XOR " ++ show v1 ++ ", " ++ show v2
    show (INC p v) = renderStringComment p $ "INC " ++ show v
    show (SETZ p v) = renderStringComment p $ "SETZ " ++ show v
    show (JMP p v) = renderStringComment p $ "JMP " ++ show v
    show (JZ p v) = renderStringComment p $ "JZ " ++ show v
    show (JNZ p v) = renderStringComment p $ "JNZ " ++ show v
    show (JE p v) = renderStringComment p $ "JE " ++ show v
    show (JNE p v) = renderStringComment p $ "JNE " ++ show v
    show (JL p v) = renderStringComment p $ "JL " ++ show v
    show (JLE p v) = renderStringComment p $ "JLE " ++ show v
    show (JG p v) = renderStringComment p $ "JG " ++ show v
    show (JGE p v) = renderStringComment p $ "JGE " ++ show v
    show (CMP p v1 v2) = renderStringComment p $ "CMP " ++ show v1 ++ ", " ++ show v2
    show (TEST p v1 v2) = renderStringComment p $ "TEST " ++ show v1 ++ ", " ++ show v2
    show (PUSH p v) = renderStringComment p $ "PUSH " ++ show v
    show (POP p v) = renderStringComment p $ "POP " ++ show v
    show (CALL p v) = renderStringComment p $ "CALL " ++ show v
    show (LEAVE p) = renderStringComment p $ "LEAVE"
    show (RET p) = renderStringComment p $ "RET"
    show (SetLabel p l) = renderStringComment p $ l ++ ":"
    show (Section p s) = renderStringComment p $  "section ." ++ s
    show (Global p s) = renderStringComment p $ "global " ++ s
    show (DB p v) = renderStringComment p $ "DB " ++ show v
    show (DW p v) = renderStringComment p $ "DW " ++ show v
    show (DD p v) = renderStringComment p $ "DD " ++ show v
    show (DQ p v) = renderStringComment p $ "DQ " ++ show v

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
    deriving (Read, Generic, Foldable, Traversable, Functor, NFData)
instance IsASM Reg


instance Show (Reg IR.IRPosition) where
    show (R11 _) = "R11"
    show (R11D _) = "R11D"
    show (R11B _) = "R11B" 
    show (R10 _) = "R10"
    show (R10D _) = "R10D"
    show (R10B _) = "R10B" 
    show (R9 _) = "R9" 
    show (R9D _) = "R9D"
    show (R9B _) = "R9B" 
    show (R8 _) = "R8" 
    show (R8D _) = "R8D"
    show (R8B _) = "R8B" 
    show (RDX _) = "RDX"
    show (EDX _) = "EDX"
    show (DL _) = "DL"
    show (RCX _) = "RCX"
    show (ECX _) = "ECX"
    show (CL _) = "CL"
    show (RAX _) = "RAX"
    show (EAX _) = "EAX"
    show (AL _) = "AL"
    show (RBP _) = "RBP"
    show (RSP _) = "RSP"
    show (RSI _) = "RSI"
    show (ESI _) = "ESI"
    show (SIL _) = "SIL"
    show (RDI _) = "RDI"
    show (EDI _) = "EDI"
    show (DIL _) = "DIL"
    show (RBX _) = "RBX"
    show (EBX _) = "EBX"
    show (BL _) = "BL"
    show (R12 _) = "R12"
    show (R12D _) = "R12D"
    show (R12B _) = "R12B"
    show (R13 _) = "R13"
    show (R13D _) = "R13D"
    show (R13B _) = "R13B"
    show (R14 _) = "R14"
    show (R14D _) = "R14D"
    show (R14B _) = "R14B"
    show (R15 _) = "R15"
    show (R15D _) = "R15D"
    show (R15B _) = "R15B"


instance Show (Value IR.IRPosition) where
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

instance Show (Program IR.IRPosition) where
    show (Program _ is) = "%include 'lib/runtime.ext'\n" ++ (concat $ map (\stmt -> show stmt ++ "\n") is)

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

regEq :: (Reg IR.IRPosition) -> (Reg IR.IRPosition) -> Bool
regEq a b = (show $ fmap (const IR.noPosIR) a) == (show $ fmap (const IR.noPosIR) b)

valueEq :: (Value IR.IRPosition) -> (Value IR.IRPosition) -> Bool
valueEq a b = (show $ fmap (const IR.noPosIR) a) == (show $ fmap (const IR.noPosIR) b)

instrEq :: (Instruction IR.IRPosition) -> (Instruction IR.IRPosition) -> Bool
instrEq a b = (show $ fmap (const IR.noPosIR) a) == (show $ fmap (const IR.noPosIR) b)

instance Eq (Value IR.IRPosition) where
    (==) = valueEq

instance Eq (Reg IR.IRPosition) where
    (==) = regEq

instance Eq (Instruction IR.IRPosition) where
    (==) = instrEq

instance Eq (Program IR.IRPosition) where
    (==) a b = (show $ fmap (const IR.noPosIR) a) == (show $ fmap (const IR.noPosIR) b)