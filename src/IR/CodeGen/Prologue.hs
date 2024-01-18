{-# LANGUAGE QuasiQuotes #-}
module IR.CodeGen.Prologue (withPrologue) where

import qualified Backend.X64.Parser.Constructor as X64
import           Data.List
import qualified Data.Map                  as Map
import           IR.Syntax.Syntax

import qualified IR.CodeGen.Emit       as Emit
import           IR.CodeGen.GenM
import           IR.Loc
import           IR.RegisterAllocation.RegisterAllocation
import           IR.Registers
import           IR.Size

withPrologue :: QIdent a -> RegisterAllocation -> CompiledMethod -> CompiledMethod
withPrologue qi rs mthd =
    let locs = fromIntegral $ numLocals rs * 8
        savedRegs = sort $ filter (\r -> X64.regType r == X64.CalleeSaved) $ usedRegs rs
        needsAlignment = odd $ length savedRegs
        prologue =
            [colouredInterferenceComment, Emit.label (labelFor qi (LabIdent "")) ""] ++
            map (\r -> Emit.push (X64.LocReg r) "") savedRegs ++
            [Emit.incrStack 8 "16 bytes alignment" | needsAlignment] ++
            [
                Emit.push (X64.LocReg X64.RBP) "",
                Emit.mov X64.Size64 (X64.LocReg X64.RSP) (X64.LocReg X64.RBP) "",
                Emit.incrStack locs "space for locals"
            ]
        -- Access to parameters passed on stack has to be offset by 8 for each saved
        -- register, including rbp. Additionally correct for alignment.
        paramOffset = 8 * (length savedRegs + 1 + if needsAlignment then 1 else 0)
        newCode = mthdCode mthd
        -- FIXME: Skipping offset!!!
        -- newCode = offsetStackParamReferences paramOffset (mthdCode mthd)
        colouredInterferenceComment = Emit.commentMultiline ("Register allocation:":lines (show $ Map.toList $ regAlloc rs))
    in mthd {mthdPrologue = map Emit.emitAsString prologue, mthdCode = newCode}

-- offsetStackParamReferences :: Int -> [String] -> [String]
-- offsetStackParamReferences offset = map go
--     where go line =
--             let pattern_ = [re|([[:space:]])([0-9]+)\(%rbp\)|]
--                 ms = line *=~ pattern_
--                 repl _ loc capt =
--                     if getCaptureOrdinal (locationCapture loc) == 2
--                         then let base = read $ capturedText capt
--                              in  Just $ show (base + offset)
--                         else Nothing
--             in  replaceAllCaptures SUB repl ms
