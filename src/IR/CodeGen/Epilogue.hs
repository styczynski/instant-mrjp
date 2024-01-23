{-# LANGUAGE FlexibleInstances #-}
module IR.CodeGen.Epilogue (withEpilogue) where

import           Data.List                 (sortOn)
import           Data.Ord                  (Down (Down))
import qualified IR.CodeGen.Emit       as Emit
import           IR.CodeGen.GenM
import           IR.RegisterAllocation.RegisterAllocation
import qualified Backend.X64.Parser.Constructor as X64

withEpilogue :: RegisterAllocation -> CompiledMethod -> CompiledMethod
withEpilogue rs mthd =
    let savedRegs = sortOn Down $ filter (\r -> X64.regType r == X64.CalleeSaved) $ usedRegs rs
        neededAlignment = odd $ length savedRegs
        epilogue =
            Emit.leave :
            [Emit.decrStack 8 | neededAlignment] ++
            map (Emit.pop . X64.LocReg) savedRegs ++
            [Emit.ret]
    in mthd {mthdEpilogue = map Emit.emitAsString epilogue}
