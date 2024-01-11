{-# LANGUAGE FlexibleInstances #-}
module IR.CodeGen.Epilogue (withEpilogue) where

import           Data.List                 (sortOn)
import           Data.Ord                  (Down (Down))
import qualified IR.CodeGen.Emit       as Emit
import           IR.CodeGen.GenM
import           IR.Loc
import           IR.RegisterAllocation.RegisterAllocation
import           IR.Registers

withEpilogue :: RegisterAllocation -> CompiledMethod -> CompiledMethod
withEpilogue rs mthd =
    let savedRegs = sortOn Down $ filter (\r -> regType r == CalleeSaved) $ usedRegs rs
        neededAlignment = odd $ length savedRegs
        epilogue =
            Emit.leave :
            [Emit.decrStack 8 | neededAlignment] ++
            map (Emit.pop . LocReg) savedRegs ++
            [Emit.ret]
    in mthd {mthdEpilogue = map Emit.emitAsString epilogue}
