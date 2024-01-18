-- Stack state during assembly generation.
module IR.CodeGen.Stack where

import qualified Backend.X64.Parser.Constructor as X64

import           Data.Int
import           IR.Size

-- The stack contains a number of reserved slots for locals
-- and an overhead of spilled variables and empty areas for alignment purposes.
data Stack = Stack {
    stackReservedSize :: Int64,
    stackOverheadSize :: Int64
}

stackNew :: Int -> Stack
stackNew locals = Stack (fromIntegral locals * 8) 0

-- Remove all values that do not have reserved locations.
stackClearOverhead :: Stack -> (Int64, Stack)
stackClearOverhead s =
    let overhead = stackOverheadSize s
        s' = s {stackOverheadSize = 0}
    in  (overhead, s')

-- Increase the stack overhead with some unidentified value.
stackPush :: X64.Size -> Stack -> Stack
stackPush size s = s {stackOverheadSize = stackOverheadSize s + X64.toBytes size}

-- Align the stack to take a multiple of 16 bytes. Returns the applied additional offset
-- and the aligned stack.
stackAlign16 :: Stack -> (Int64, Stack)
stackAlign16 s = let misalignment = 16 - stackSize s `mod` 16
                     x = if misalignment == 16 then 0 else misalignment
                 in (x, s {stackOverheadSize = stackOverheadSize s + x})

-- Size of the entire stack counting from 0 downwards.
stackSize :: Stack -> Int64
stackSize s = stackReservedSize s + stackOverheadSize s
