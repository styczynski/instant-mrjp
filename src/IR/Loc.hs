-- Data type for representing variable locations during execution
-- such as register or memory.
module IR.Loc where

import           Data.Int
import           IR.Registers
import           IR.Size
import qualified Backend.X64.Parser.Constructor as X64

-- data Loc = LocImm Int32
--          | LocImm64 Int64
--          | LocReg X64.Reg
--          | LocPtr X64.Reg Int64
--          | LocPtrCmplx { ptrBase :: X64.Reg, ptrIdx :: X64.Reg, ptrOffset :: Int64, ptrScale :: X64.Size}
--          | LocLabel String
--     deriving (Eq, Show)

-- isReg :: Loc -> Bool
-- isReg loc = case loc of
--     LocReg _ -> True
--     _        -> False

-- asReg :: Loc -> Reg
-- asReg loc = case loc of
--     LocReg r -> r
--     _        -> error "asReg: not a reg"

-- -- For n-th argument of a function (starting from zero) give
-- -- the location it is stored in. Assumes only the return
-- -- address is stored after arguments on stack, the consumer
-- -- must correct for the actual offset.
-- argLoc :: Integer -> Loc
-- argLoc idx = case idx of
--     0 -> LocReg rdi
--     1 -> LocReg rsi
--     2 -> LocReg rdx
--     3 -> LocReg rcx
--     4 -> LocReg r8
--     5 -> LocReg r9
--     _ -> LocPtr rbp ((fromInteger idx - 6) * 8 + 8)
