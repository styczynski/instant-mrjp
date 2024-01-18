
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
module Backend.X64.Parser.Constructor(runASMGeneratorT
, ASMGenerator
, ASMGeneratorT
, dataDef
, DataDef(..)
, Data(..)
, Loc(..)
, Size(..)
, Reg(..)
, Loc(..)
, Instr(..)
, add
, and
, cmp
, idiv
, imul
, lea
, mov
, sub
, test
, xor
, xchg
, sal
, sar
, neg
, sete
, setg
, setge
, setl
, setle
, setne
, pop
, push
, leave
, ret
, cdq
, jmp
, jz) where

import Data.Generics.Product
import Data.Generics.Sum
import GHC.Generics (Generic)
import Data.Typeable
import qualified Data.Data as D

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer.Lazy

import Prelude hiding (and)
import Data.Int

import qualified Backend.X64.Parser.Gen.AbsXGAS as Syntax
import qualified Backend.X64.Parser.Gen.PrintXGAS as Printer

-- Data defintions
data DataDef
	= DataGlobal String | DataDef String [Data]
	deriving (Eq, Ord, Show, Read, D.Data, Typeable, Generic)

data Data
	= DataStr String
	| Data64I Integer
	| Data32I Integer
	| Data64From String
	| Data32From String
	deriving (Eq, Ord, Show, Read, D.Data, Typeable, Generic)

convertDataDef :: a -> DataDef -> Syntax.AsmDataDef' a
convertDataDef pos (DataGlobal label) = Syntax.AsmDataGlobal pos $ Syntax.Label label
convertDataDef pos (DataDef label datas) = Syntax.AsmDataDef pos (Syntax.Label label) (map (convertData pos) datas)
	where
		convertData :: a -> Data -> Syntax.Data' a
		convertData pos (DataStr str) = Syntax.DataString pos str
		convertData pos (Data64I val) = Syntax.Data64 pos $ Syntax.ConstInt pos val
		convertData pos (Data32I val) = Syntax.Data32 pos $ Syntax.ConstInt pos val
		convertData pos (Data64From label) = Syntax.Data64 pos $ Syntax.ConstLabel pos $ Syntax.Label label
		convertData pos (Data32From label) = Syntax.Data32 pos $ Syntax.ConstLabel pos $ Syntax.Label label


-- Error definition
data GeneratorError a =
	EDataMemToMemTransfer a Loc Loc
	| EDataUnexpectedSize a Size
	| EDataUnexpectedLocation a Loc
	| ENoOutputCodeGenerated
	| ENonRegisterLocationGiven a Loc
	deriving (Eq, Ord, Show, Read, Generic, Typeable)

generatorFail :: (Monad m) => GeneratorError a -> ASMGeneratorT a anno m v
generatorFail e = throwError e

-- Assembly generator definition
type ASMGenerator a anno v = (WriterT (GeneratorOut a anno) (Except (GeneratorError a))) v
type ASMGeneratorT a anno (m :: * -> *) = (WriterT (GeneratorOut a anno) (ExceptT (GeneratorError a) m))

data GeneratorOut a anno = GeneratorOut (Maybe a) [Instr a anno] [Syntax.AsmDataDef' a]

instance Semigroup (GeneratorOut a anno) where
	(<>) (GeneratorOut Nothing instr1 defs1) (GeneratorOut (Just firstPos2) instr2 defs2) = GeneratorOut (Just firstPos2) (instr1 <> instr2) (defs1 <> defs2)
	(<>) (GeneratorOut (Just firstPos1) instr1 defs1) (GeneratorOut Nothing instr2 defs2) = GeneratorOut (Just firstPos1) (instr1 <> instr2) (defs1 <> defs2)
	(<>) (GeneratorOut firstPos1 instr1 defs1) (GeneratorOut _ instr2 defs2) = GeneratorOut firstPos1 (instr1 <> instr2) (defs1 <> defs2)

instance Monoid (GeneratorOut a anno) where
	mempty = GeneratorOut Nothing [] []

_emitInstr :: (Monad m) => a -> Instr a anno -> ASMGeneratorT a anno m ()
_emitInstr pos instr = tell $ GeneratorOut (Just pos) [instr] []

_emitDef :: (Monad m) => a -> Syntax.AsmDataDef' a -> ASMGeneratorT a anno m ()
_emitDef pos def = tell $ GeneratorOut (Just pos) [] [def]

runASMGeneratorT :: (Monad m) => (ASMGeneratorT a anno m v) -> [String] -> m (Either (GeneratorError a) (String, v))
runASMGeneratorT generator externs = do
	genResult <- runExceptT (runWriterT $ generator)
	outResult <- runExceptT (runWriterT $ generateOut genResult)
	case outResult  of
		Left err -> return $ Left err
		Right ((result, fullProg), _) -> do
			let assemblyCodeStr = Printer.printTree fullProg
			return $ Right (assemblyCodeStr, result)
	where
		generateOut :: (Monad m) => Either (GeneratorError a) (v, GeneratorOut a anno) -> (ASMGeneratorT a anno m (v, Syntax.AsmProgram' a))
		generateOut r =
			case r of
				Left err -> generatorFail err
				Right (_, GeneratorOut Nothing _ _) -> generatorFail ENoOutputCodeGenerated
				Right (result, GeneratorOut (Just pos) instrs defs) -> do
					instrs' <- mapM _convertInstr instrs
					let topDirectives = map (Syntax.Extern pos . Syntax.Label) externs
					let fullProg = Syntax.AsmProgram pos topDirectives (Syntax.SectionData pos defs) (Syntax.SectionCode pos instrs')
					return (result, fullProg)

-- Registers

data Reg = RAX| RBX| RCX| RDX| RDI| RSI| RSP| RBP| R8| R9| R10| R11| R12| R13| R14| R15
	deriving (Eq, Ord, Show, Read, Generic, Typeable)


data Loc = LocConst Integer | LocReg Reg | LocMem (Reg, Int64) | LocMemOffset { ptrBase :: Reg, ptrIdx :: Reg, ptrOffset :: Int64, ptrScale :: Size }
	deriving (Eq, Ord, Show, Read, Generic, Typeable)

data Annotation a anno = NoAnnotation a | Annotation a anno
	deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor, Typeable)

_locToSource :: a -> Size -> Loc -> Syntax.Source' a
_locToSource pos _ (LocConst val) = Syntax.FromConst pos val
_locToSource pos Size64 (LocReg RAX) = Syntax.FromReg64 pos $ Syntax.RAX pos
_locToSource pos Size64 (LocMem (RAX, offset)) = Syntax.FromMem64 pos (fromIntegral offset) $ Syntax.RAX pos
_locToSource pos Size64 (LocReg RBX) = Syntax.FromReg64 pos $ Syntax.RBX pos
_locToSource pos Size64 (LocMem (RBX, offset)) = Syntax.FromMem64 pos (fromIntegral offset) $ Syntax.RBX pos
_locToSource pos Size64 (LocReg RCX) = Syntax.FromReg64 pos $ Syntax.RCX pos
_locToSource pos Size64 (LocMem (RCX, offset)) = Syntax.FromMem64 pos (fromIntegral offset) $ Syntax.RCX pos
_locToSource pos Size64 (LocReg RDX) = Syntax.FromReg64 pos $ Syntax.RDX pos
_locToSource pos Size64 (LocMem (RDX, offset)) = Syntax.FromMem64 pos (fromIntegral offset) $ Syntax.RDX pos
_locToSource pos Size64 (LocReg RDI) = Syntax.FromReg64 pos $ Syntax.RDI pos
_locToSource pos Size64 (LocMem (RDI, offset)) = Syntax.FromMem64 pos (fromIntegral offset) $ Syntax.RDI pos
_locToSource pos Size64 (LocReg RSI) = Syntax.FromReg64 pos $ Syntax.RSI pos
_locToSource pos Size64 (LocMem (RSI, offset)) = Syntax.FromMem64 pos (fromIntegral offset) $ Syntax.RSI pos
_locToSource pos Size64 (LocReg RSP) = Syntax.FromReg64 pos $ Syntax.RSP pos
_locToSource pos Size64 (LocMem (RSP, offset)) = Syntax.FromMem64 pos (fromIntegral offset) $ Syntax.RSP pos
_locToSource pos Size64 (LocReg RBP) = Syntax.FromReg64 pos $ Syntax.RBP pos
_locToSource pos Size64 (LocMem (RBP, offset)) = Syntax.FromMem64 pos (fromIntegral offset) $ Syntax.RBP pos
_locToSource pos Size64 (LocReg R8) = Syntax.FromReg64 pos $ Syntax.R8 pos
_locToSource pos Size64 (LocMem (R8, offset)) = Syntax.FromMem64 pos (fromIntegral offset) $ Syntax.R8 pos
_locToSource pos Size64 (LocReg R9) = Syntax.FromReg64 pos $ Syntax.R9 pos
_locToSource pos Size64 (LocMem (R9, offset)) = Syntax.FromMem64 pos (fromIntegral offset) $ Syntax.R9 pos
_locToSource pos Size64 (LocReg R10) = Syntax.FromReg64 pos $ Syntax.R10 pos
_locToSource pos Size64 (LocMem (R10, offset)) = Syntax.FromMem64 pos (fromIntegral offset) $ Syntax.R10 pos
_locToSource pos Size64 (LocReg R11) = Syntax.FromReg64 pos $ Syntax.R11 pos
_locToSource pos Size64 (LocMem (R11, offset)) = Syntax.FromMem64 pos (fromIntegral offset) $ Syntax.R11 pos
_locToSource pos Size64 (LocReg R12) = Syntax.FromReg64 pos $ Syntax.R12 pos
_locToSource pos Size64 (LocMem (R12, offset)) = Syntax.FromMem64 pos (fromIntegral offset) $ Syntax.R12 pos
_locToSource pos Size64 (LocReg R13) = Syntax.FromReg64 pos $ Syntax.R13 pos
_locToSource pos Size64 (LocMem (R13, offset)) = Syntax.FromMem64 pos (fromIntegral offset) $ Syntax.R13 pos
_locToSource pos Size64 (LocReg R14) = Syntax.FromReg64 pos $ Syntax.R14 pos
_locToSource pos Size64 (LocMem (R14, offset)) = Syntax.FromMem64 pos (fromIntegral offset) $ Syntax.R14 pos
_locToSource pos Size64 (LocReg R15) = Syntax.FromReg64 pos $ Syntax.R15 pos
_locToSource pos Size64 (LocMem (R15, offset)) = Syntax.FromMem64 pos (fromIntegral offset) $ Syntax.R15 pos
_locToSource pos Size32 (LocReg RAX) = Syntax.FromReg32 pos $ Syntax.EAX pos
_locToSource pos Size32 (LocMem (RAX, offset)) = Syntax.FromMem32 pos (fromIntegral offset) $ Syntax.EAX pos
_locToSource pos Size32 (LocReg RBX) = Syntax.FromReg32 pos $ Syntax.EBX pos
_locToSource pos Size32 (LocMem (RBX, offset)) = Syntax.FromMem32 pos (fromIntegral offset) $ Syntax.EBX pos
_locToSource pos Size32 (LocReg RCX) = Syntax.FromReg32 pos $ Syntax.ECX pos
_locToSource pos Size32 (LocMem (RCX, offset)) = Syntax.FromMem32 pos (fromIntegral offset) $ Syntax.ECX pos
_locToSource pos Size32 (LocReg RDX) = Syntax.FromReg32 pos $ Syntax.EDX pos
_locToSource pos Size32 (LocMem (RDX, offset)) = Syntax.FromMem32 pos (fromIntegral offset) $ Syntax.EDX pos
_locToSource pos Size32 (LocReg RDI) = Syntax.FromReg32 pos $ Syntax.EDI pos
_locToSource pos Size32 (LocMem (RDI, offset)) = Syntax.FromMem32 pos (fromIntegral offset) $ Syntax.EDI pos
_locToSource pos Size32 (LocReg RSI) = Syntax.FromReg32 pos $ Syntax.ESI pos
_locToSource pos Size32 (LocMem (RSI, offset)) = Syntax.FromMem32 pos (fromIntegral offset) $ Syntax.ESI pos
_locToSource pos Size32 (LocReg RSP) = Syntax.FromReg32 pos $ Syntax.ESP pos
_locToSource pos Size32 (LocMem (RSP, offset)) = Syntax.FromMem32 pos (fromIntegral offset) $ Syntax.ESP pos
_locToSource pos Size32 (LocReg RBP) = Syntax.FromReg32 pos $ Syntax.EBP pos
_locToSource pos Size32 (LocMem (RBP, offset)) = Syntax.FromMem32 pos (fromIntegral offset) $ Syntax.EBP pos
_locToSource pos Size32 (LocReg R8) = Syntax.FromReg32 pos $ Syntax.R8D pos
_locToSource pos Size32 (LocMem (R8, offset)) = Syntax.FromMem32 pos (fromIntegral offset) $ Syntax.R8D pos
_locToSource pos Size32 (LocReg R9) = Syntax.FromReg32 pos $ Syntax.R9D pos
_locToSource pos Size32 (LocMem (R9, offset)) = Syntax.FromMem32 pos (fromIntegral offset) $ Syntax.R9D pos
_locToSource pos Size32 (LocReg R10) = Syntax.FromReg32 pos $ Syntax.R10D pos
_locToSource pos Size32 (LocMem (R10, offset)) = Syntax.FromMem32 pos (fromIntegral offset) $ Syntax.R10D pos
_locToSource pos Size32 (LocReg R11) = Syntax.FromReg32 pos $ Syntax.R11D pos
_locToSource pos Size32 (LocMem (R11, offset)) = Syntax.FromMem32 pos (fromIntegral offset) $ Syntax.R11D pos
_locToSource pos Size32 (LocReg R12) = Syntax.FromReg32 pos $ Syntax.R12D pos
_locToSource pos Size32 (LocMem (R12, offset)) = Syntax.FromMem32 pos (fromIntegral offset) $ Syntax.R12D pos
_locToSource pos Size32 (LocReg R13) = Syntax.FromReg32 pos $ Syntax.R13D pos
_locToSource pos Size32 (LocMem (R13, offset)) = Syntax.FromMem32 pos (fromIntegral offset) $ Syntax.R13D pos
_locToSource pos Size32 (LocReg R14) = Syntax.FromReg32 pos $ Syntax.R14D pos
_locToSource pos Size32 (LocMem (R14, offset)) = Syntax.FromMem32 pos (fromIntegral offset) $ Syntax.R14D pos
_locToSource pos Size32 (LocReg R15) = Syntax.FromReg32 pos $ Syntax.R15D pos
_locToSource pos Size32 (LocMem (R15, offset)) = Syntax.FromMem32 pos (fromIntegral offset) $ Syntax.R15D pos
_locToSource pos Size16 (LocReg RAX) = Syntax.FromReg16 pos $ Syntax.AX pos
_locToSource pos Size16 (LocMem (RAX, offset)) = Syntax.FromMem16 pos (fromIntegral offset) $ Syntax.AX pos
_locToSource pos Size16 (LocReg RBX) = Syntax.FromReg16 pos $ Syntax.BX pos
_locToSource pos Size16 (LocMem (RBX, offset)) = Syntax.FromMem16 pos (fromIntegral offset) $ Syntax.BX pos
_locToSource pos Size16 (LocReg RCX) = Syntax.FromReg16 pos $ Syntax.CX pos
_locToSource pos Size16 (LocMem (RCX, offset)) = Syntax.FromMem16 pos (fromIntegral offset) $ Syntax.CX pos
_locToSource pos Size16 (LocReg RDX) = Syntax.FromReg16 pos $ Syntax.DX pos
_locToSource pos Size16 (LocMem (RDX, offset)) = Syntax.FromMem16 pos (fromIntegral offset) $ Syntax.DX pos
_locToSource pos Size16 (LocReg RDI) = Syntax.FromReg16 pos $ Syntax.DI pos
_locToSource pos Size16 (LocMem (RDI, offset)) = Syntax.FromMem16 pos (fromIntegral offset) $ Syntax.DI pos
_locToSource pos Size16 (LocReg RSI) = Syntax.FromReg16 pos $ Syntax.SI pos
_locToSource pos Size16 (LocMem (RSI, offset)) = Syntax.FromMem16 pos (fromIntegral offset) $ Syntax.SI pos
_locToSource pos Size16 (LocReg RSP) = Syntax.FromReg16 pos $ Syntax.SP pos
_locToSource pos Size16 (LocMem (RSP, offset)) = Syntax.FromMem16 pos (fromIntegral offset) $ Syntax.SP pos
_locToSource pos Size16 (LocReg RBP) = Syntax.FromReg16 pos $ Syntax.BP pos
_locToSource pos Size16 (LocMem (RBP, offset)) = Syntax.FromMem16 pos (fromIntegral offset) $ Syntax.BP pos
_locToSource pos Size16 (LocReg R8) = Syntax.FromReg16 pos $ Syntax.R8W pos
_locToSource pos Size16 (LocMem (R8, offset)) = Syntax.FromMem16 pos (fromIntegral offset) $ Syntax.R8W pos
_locToSource pos Size16 (LocReg R9) = Syntax.FromReg16 pos $ Syntax.R9W pos
_locToSource pos Size16 (LocMem (R9, offset)) = Syntax.FromMem16 pos (fromIntegral offset) $ Syntax.R9W pos
_locToSource pos Size16 (LocReg R10) = Syntax.FromReg16 pos $ Syntax.R10W pos
_locToSource pos Size16 (LocMem (R10, offset)) = Syntax.FromMem16 pos (fromIntegral offset) $ Syntax.R10W pos
_locToSource pos Size16 (LocReg R11) = Syntax.FromReg16 pos $ Syntax.R11W pos
_locToSource pos Size16 (LocMem (R11, offset)) = Syntax.FromMem16 pos (fromIntegral offset) $ Syntax.R11W pos
_locToSource pos Size16 (LocReg R12) = Syntax.FromReg16 pos $ Syntax.R12W pos
_locToSource pos Size16 (LocMem (R12, offset)) = Syntax.FromMem16 pos (fromIntegral offset) $ Syntax.R12W pos
_locToSource pos Size16 (LocReg R13) = Syntax.FromReg16 pos $ Syntax.R13W pos
_locToSource pos Size16 (LocMem (R13, offset)) = Syntax.FromMem16 pos (fromIntegral offset) $ Syntax.R13W pos
_locToSource pos Size16 (LocReg R14) = Syntax.FromReg16 pos $ Syntax.R14W pos
_locToSource pos Size16 (LocMem (R14, offset)) = Syntax.FromMem16 pos (fromIntegral offset) $ Syntax.R14W pos
_locToSource pos Size16 (LocReg R15) = Syntax.FromReg16 pos $ Syntax.R15W pos
_locToSource pos Size16 (LocMem (R15, offset)) = Syntax.FromMem16 pos (fromIntegral offset) $ Syntax.R15W pos
_locToSource pos Size8 (LocReg RAX) = Syntax.FromReg8 pos $ Syntax.AL pos
_locToSource pos Size8 (LocMem (RAX, offset)) = Syntax.FromMem8 pos (fromIntegral offset) $ Syntax.AL pos
_locToSource pos Size8 (LocReg RBX) = Syntax.FromReg8 pos $ Syntax.BL pos
_locToSource pos Size8 (LocMem (RBX, offset)) = Syntax.FromMem8 pos (fromIntegral offset) $ Syntax.BL pos
_locToSource pos Size8 (LocReg RCX) = Syntax.FromReg8 pos $ Syntax.CL pos
_locToSource pos Size8 (LocMem (RCX, offset)) = Syntax.FromMem8 pos (fromIntegral offset) $ Syntax.CL pos
_locToSource pos Size8 (LocReg RDX) = Syntax.FromReg8 pos $ Syntax.DL pos
_locToSource pos Size8 (LocMem (RDX, offset)) = Syntax.FromMem8 pos (fromIntegral offset) $ Syntax.DL pos
_locToSource pos Size8 (LocReg RDI) = Syntax.FromReg8 pos $ Syntax.DIL pos
_locToSource pos Size8 (LocMem (RDI, offset)) = Syntax.FromMem8 pos (fromIntegral offset) $ Syntax.DIL pos
_locToSource pos Size8 (LocReg RSI) = Syntax.FromReg8 pos $ Syntax.SIL pos
_locToSource pos Size8 (LocMem (RSI, offset)) = Syntax.FromMem8 pos (fromIntegral offset) $ Syntax.SIL pos
_locToSource pos Size8 (LocReg RSP) = Syntax.FromReg8 pos $ Syntax.SPL pos
_locToSource pos Size8 (LocMem (RSP, offset)) = Syntax.FromMem8 pos (fromIntegral offset) $ Syntax.SPL pos
_locToSource pos Size8 (LocReg RBP) = Syntax.FromReg8 pos $ Syntax.BPL pos
_locToSource pos Size8 (LocMem (RBP, offset)) = Syntax.FromMem8 pos (fromIntegral offset) $ Syntax.BPL pos
_locToSource pos Size8 (LocReg R8) = Syntax.FromReg8 pos $ Syntax.R8B pos
_locToSource pos Size8 (LocMem (R8, offset)) = Syntax.FromMem8 pos (fromIntegral offset) $ Syntax.R8B pos
_locToSource pos Size8 (LocReg R9) = Syntax.FromReg8 pos $ Syntax.R9B pos
_locToSource pos Size8 (LocMem (R9, offset)) = Syntax.FromMem8 pos (fromIntegral offset) $ Syntax.R9B pos
_locToSource pos Size8 (LocReg R10) = Syntax.FromReg8 pos $ Syntax.R10B pos
_locToSource pos Size8 (LocMem (R10, offset)) = Syntax.FromMem8 pos (fromIntegral offset) $ Syntax.R10B pos
_locToSource pos Size8 (LocReg R11) = Syntax.FromReg8 pos $ Syntax.R11B pos
_locToSource pos Size8 (LocMem (R11, offset)) = Syntax.FromMem8 pos (fromIntegral offset) $ Syntax.R11B pos
_locToSource pos Size8 (LocReg R12) = Syntax.FromReg8 pos $ Syntax.R12B pos
_locToSource pos Size8 (LocMem (R12, offset)) = Syntax.FromMem8 pos (fromIntegral offset) $ Syntax.R12B pos
_locToSource pos Size8 (LocReg R13) = Syntax.FromReg8 pos $ Syntax.R13B pos
_locToSource pos Size8 (LocMem (R13, offset)) = Syntax.FromMem8 pos (fromIntegral offset) $ Syntax.R13B pos
_locToSource pos Size8 (LocReg R14) = Syntax.FromReg8 pos $ Syntax.R14B pos
_locToSource pos Size8 (LocMem (R14, offset)) = Syntax.FromMem8 pos (fromIntegral offset) $ Syntax.R14B pos
_locToSource pos Size8 (LocReg R15) = Syntax.FromReg8 pos $ Syntax.R15B pos
_locToSource pos Size8 (LocMem (R15, offset)) = Syntax.FromMem8 pos (fromIntegral offset) $ Syntax.R15B pos
_locToTarget :: a -> Size -> Loc -> Syntax.Target' a
_locToTarget pos Size64 (LocReg RAX) = Syntax.ToReg64 pos $ Syntax.RAX pos
_locToTarget pos Size64 (LocMem (RAX, offset)) = Syntax.ToMem64 pos (fromIntegral offset) $ Syntax.RAX pos
_locToTarget pos Size64 (LocReg RBX) = Syntax.ToReg64 pos $ Syntax.RBX pos
_locToTarget pos Size64 (LocMem (RBX, offset)) = Syntax.ToMem64 pos (fromIntegral offset) $ Syntax.RBX pos
_locToTarget pos Size64 (LocReg RCX) = Syntax.ToReg64 pos $ Syntax.RCX pos
_locToTarget pos Size64 (LocMem (RCX, offset)) = Syntax.ToMem64 pos (fromIntegral offset) $ Syntax.RCX pos
_locToTarget pos Size64 (LocReg RDX) = Syntax.ToReg64 pos $ Syntax.RDX pos
_locToTarget pos Size64 (LocMem (RDX, offset)) = Syntax.ToMem64 pos (fromIntegral offset) $ Syntax.RDX pos
_locToTarget pos Size64 (LocReg RDI) = Syntax.ToReg64 pos $ Syntax.RDI pos
_locToTarget pos Size64 (LocMem (RDI, offset)) = Syntax.ToMem64 pos (fromIntegral offset) $ Syntax.RDI pos
_locToTarget pos Size64 (LocReg RSI) = Syntax.ToReg64 pos $ Syntax.RSI pos
_locToTarget pos Size64 (LocMem (RSI, offset)) = Syntax.ToMem64 pos (fromIntegral offset) $ Syntax.RSI pos
_locToTarget pos Size64 (LocReg RSP) = Syntax.ToReg64 pos $ Syntax.RSP pos
_locToTarget pos Size64 (LocMem (RSP, offset)) = Syntax.ToMem64 pos (fromIntegral offset) $ Syntax.RSP pos
_locToTarget pos Size64 (LocReg RBP) = Syntax.ToReg64 pos $ Syntax.RBP pos
_locToTarget pos Size64 (LocMem (RBP, offset)) = Syntax.ToMem64 pos (fromIntegral offset) $ Syntax.RBP pos
_locToTarget pos Size64 (LocReg R8) = Syntax.ToReg64 pos $ Syntax.R8 pos
_locToTarget pos Size64 (LocMem (R8, offset)) = Syntax.ToMem64 pos (fromIntegral offset) $ Syntax.R8 pos
_locToTarget pos Size64 (LocReg R9) = Syntax.ToReg64 pos $ Syntax.R9 pos
_locToTarget pos Size64 (LocMem (R9, offset)) = Syntax.ToMem64 pos (fromIntegral offset) $ Syntax.R9 pos
_locToTarget pos Size64 (LocReg R10) = Syntax.ToReg64 pos $ Syntax.R10 pos
_locToTarget pos Size64 (LocMem (R10, offset)) = Syntax.ToMem64 pos (fromIntegral offset) $ Syntax.R10 pos
_locToTarget pos Size64 (LocReg R11) = Syntax.ToReg64 pos $ Syntax.R11 pos
_locToTarget pos Size64 (LocMem (R11, offset)) = Syntax.ToMem64 pos (fromIntegral offset) $ Syntax.R11 pos
_locToTarget pos Size64 (LocReg R12) = Syntax.ToReg64 pos $ Syntax.R12 pos
_locToTarget pos Size64 (LocMem (R12, offset)) = Syntax.ToMem64 pos (fromIntegral offset) $ Syntax.R12 pos
_locToTarget pos Size64 (LocReg R13) = Syntax.ToReg64 pos $ Syntax.R13 pos
_locToTarget pos Size64 (LocMem (R13, offset)) = Syntax.ToMem64 pos (fromIntegral offset) $ Syntax.R13 pos
_locToTarget pos Size64 (LocReg R14) = Syntax.ToReg64 pos $ Syntax.R14 pos
_locToTarget pos Size64 (LocMem (R14, offset)) = Syntax.ToMem64 pos (fromIntegral offset) $ Syntax.R14 pos
_locToTarget pos Size64 (LocReg R15) = Syntax.ToReg64 pos $ Syntax.R15 pos
_locToTarget pos Size64 (LocMem (R15, offset)) = Syntax.ToMem64 pos (fromIntegral offset) $ Syntax.R15 pos
_locToTarget pos Size32 (LocReg RAX) = Syntax.ToReg32 pos $ Syntax.EAX pos
_locToTarget pos Size32 (LocMem (RAX, offset)) = Syntax.ToMem32 pos (fromIntegral offset) $ Syntax.EAX pos
_locToTarget pos Size32 (LocReg RBX) = Syntax.ToReg32 pos $ Syntax.EBX pos
_locToTarget pos Size32 (LocMem (RBX, offset)) = Syntax.ToMem32 pos (fromIntegral offset) $ Syntax.EBX pos
_locToTarget pos Size32 (LocReg RCX) = Syntax.ToReg32 pos $ Syntax.ECX pos
_locToTarget pos Size32 (LocMem (RCX, offset)) = Syntax.ToMem32 pos (fromIntegral offset) $ Syntax.ECX pos
_locToTarget pos Size32 (LocReg RDX) = Syntax.ToReg32 pos $ Syntax.EDX pos
_locToTarget pos Size32 (LocMem (RDX, offset)) = Syntax.ToMem32 pos (fromIntegral offset) $ Syntax.EDX pos
_locToTarget pos Size32 (LocReg RDI) = Syntax.ToReg32 pos $ Syntax.EDI pos
_locToTarget pos Size32 (LocMem (RDI, offset)) = Syntax.ToMem32 pos (fromIntegral offset) $ Syntax.EDI pos
_locToTarget pos Size32 (LocReg RSI) = Syntax.ToReg32 pos $ Syntax.ESI pos
_locToTarget pos Size32 (LocMem (RSI, offset)) = Syntax.ToMem32 pos (fromIntegral offset) $ Syntax.ESI pos
_locToTarget pos Size32 (LocReg RSP) = Syntax.ToReg32 pos $ Syntax.ESP pos
_locToTarget pos Size32 (LocMem (RSP, offset)) = Syntax.ToMem32 pos (fromIntegral offset) $ Syntax.ESP pos
_locToTarget pos Size32 (LocReg RBP) = Syntax.ToReg32 pos $ Syntax.EBP pos
_locToTarget pos Size32 (LocMem (RBP, offset)) = Syntax.ToMem32 pos (fromIntegral offset) $ Syntax.EBP pos
_locToTarget pos Size32 (LocReg R8) = Syntax.ToReg32 pos $ Syntax.R8D pos
_locToTarget pos Size32 (LocMem (R8, offset)) = Syntax.ToMem32 pos (fromIntegral offset) $ Syntax.R8D pos
_locToTarget pos Size32 (LocReg R9) = Syntax.ToReg32 pos $ Syntax.R9D pos
_locToTarget pos Size32 (LocMem (R9, offset)) = Syntax.ToMem32 pos (fromIntegral offset) $ Syntax.R9D pos
_locToTarget pos Size32 (LocReg R10) = Syntax.ToReg32 pos $ Syntax.R10D pos
_locToTarget pos Size32 (LocMem (R10, offset)) = Syntax.ToMem32 pos (fromIntegral offset) $ Syntax.R10D pos
_locToTarget pos Size32 (LocReg R11) = Syntax.ToReg32 pos $ Syntax.R11D pos
_locToTarget pos Size32 (LocMem (R11, offset)) = Syntax.ToMem32 pos (fromIntegral offset) $ Syntax.R11D pos
_locToTarget pos Size32 (LocReg R12) = Syntax.ToReg32 pos $ Syntax.R12D pos
_locToTarget pos Size32 (LocMem (R12, offset)) = Syntax.ToMem32 pos (fromIntegral offset) $ Syntax.R12D pos
_locToTarget pos Size32 (LocReg R13) = Syntax.ToReg32 pos $ Syntax.R13D pos
_locToTarget pos Size32 (LocMem (R13, offset)) = Syntax.ToMem32 pos (fromIntegral offset) $ Syntax.R13D pos
_locToTarget pos Size32 (LocReg R14) = Syntax.ToReg32 pos $ Syntax.R14D pos
_locToTarget pos Size32 (LocMem (R14, offset)) = Syntax.ToMem32 pos (fromIntegral offset) $ Syntax.R14D pos
_locToTarget pos Size32 (LocReg R15) = Syntax.ToReg32 pos $ Syntax.R15D pos
_locToTarget pos Size32 (LocMem (R15, offset)) = Syntax.ToMem32 pos (fromIntegral offset) $ Syntax.R15D pos
_locToTarget pos Size16 (LocReg RAX) = Syntax.ToReg16 pos $ Syntax.AX pos
_locToTarget pos Size16 (LocMem (RAX, offset)) = Syntax.ToMem16 pos (fromIntegral offset) $ Syntax.AX pos
_locToTarget pos Size16 (LocReg RBX) = Syntax.ToReg16 pos $ Syntax.BX pos
_locToTarget pos Size16 (LocMem (RBX, offset)) = Syntax.ToMem16 pos (fromIntegral offset) $ Syntax.BX pos
_locToTarget pos Size16 (LocReg RCX) = Syntax.ToReg16 pos $ Syntax.CX pos
_locToTarget pos Size16 (LocMem (RCX, offset)) = Syntax.ToMem16 pos (fromIntegral offset) $ Syntax.CX pos
_locToTarget pos Size16 (LocReg RDX) = Syntax.ToReg16 pos $ Syntax.DX pos
_locToTarget pos Size16 (LocMem (RDX, offset)) = Syntax.ToMem16 pos (fromIntegral offset) $ Syntax.DX pos
_locToTarget pos Size16 (LocReg RDI) = Syntax.ToReg16 pos $ Syntax.DI pos
_locToTarget pos Size16 (LocMem (RDI, offset)) = Syntax.ToMem16 pos (fromIntegral offset) $ Syntax.DI pos
_locToTarget pos Size16 (LocReg RSI) = Syntax.ToReg16 pos $ Syntax.SI pos
_locToTarget pos Size16 (LocMem (RSI, offset)) = Syntax.ToMem16 pos (fromIntegral offset) $ Syntax.SI pos
_locToTarget pos Size16 (LocReg RSP) = Syntax.ToReg16 pos $ Syntax.SP pos
_locToTarget pos Size16 (LocMem (RSP, offset)) = Syntax.ToMem16 pos (fromIntegral offset) $ Syntax.SP pos
_locToTarget pos Size16 (LocReg RBP) = Syntax.ToReg16 pos $ Syntax.BP pos
_locToTarget pos Size16 (LocMem (RBP, offset)) = Syntax.ToMem16 pos (fromIntegral offset) $ Syntax.BP pos
_locToTarget pos Size16 (LocReg R8) = Syntax.ToReg16 pos $ Syntax.R8W pos
_locToTarget pos Size16 (LocMem (R8, offset)) = Syntax.ToMem16 pos (fromIntegral offset) $ Syntax.R8W pos
_locToTarget pos Size16 (LocReg R9) = Syntax.ToReg16 pos $ Syntax.R9W pos
_locToTarget pos Size16 (LocMem (R9, offset)) = Syntax.ToMem16 pos (fromIntegral offset) $ Syntax.R9W pos
_locToTarget pos Size16 (LocReg R10) = Syntax.ToReg16 pos $ Syntax.R10W pos
_locToTarget pos Size16 (LocMem (R10, offset)) = Syntax.ToMem16 pos (fromIntegral offset) $ Syntax.R10W pos
_locToTarget pos Size16 (LocReg R11) = Syntax.ToReg16 pos $ Syntax.R11W pos
_locToTarget pos Size16 (LocMem (R11, offset)) = Syntax.ToMem16 pos (fromIntegral offset) $ Syntax.R11W pos
_locToTarget pos Size16 (LocReg R12) = Syntax.ToReg16 pos $ Syntax.R12W pos
_locToTarget pos Size16 (LocMem (R12, offset)) = Syntax.ToMem16 pos (fromIntegral offset) $ Syntax.R12W pos
_locToTarget pos Size16 (LocReg R13) = Syntax.ToReg16 pos $ Syntax.R13W pos
_locToTarget pos Size16 (LocMem (R13, offset)) = Syntax.ToMem16 pos (fromIntegral offset) $ Syntax.R13W pos
_locToTarget pos Size16 (LocReg R14) = Syntax.ToReg16 pos $ Syntax.R14W pos
_locToTarget pos Size16 (LocMem (R14, offset)) = Syntax.ToMem16 pos (fromIntegral offset) $ Syntax.R14W pos
_locToTarget pos Size16 (LocReg R15) = Syntax.ToReg16 pos $ Syntax.R15W pos
_locToTarget pos Size16 (LocMem (R15, offset)) = Syntax.ToMem16 pos (fromIntegral offset) $ Syntax.R15W pos
_locToTarget pos Size8 (LocReg RAX) = Syntax.ToReg8 pos $ Syntax.AL pos
_locToTarget pos Size8 (LocMem (RAX, offset)) = Syntax.ToMem8 pos (fromIntegral offset) $ Syntax.AL pos
_locToTarget pos Size8 (LocReg RBX) = Syntax.ToReg8 pos $ Syntax.BL pos
_locToTarget pos Size8 (LocMem (RBX, offset)) = Syntax.ToMem8 pos (fromIntegral offset) $ Syntax.BL pos
_locToTarget pos Size8 (LocReg RCX) = Syntax.ToReg8 pos $ Syntax.CL pos
_locToTarget pos Size8 (LocMem (RCX, offset)) = Syntax.ToMem8 pos (fromIntegral offset) $ Syntax.CL pos
_locToTarget pos Size8 (LocReg RDX) = Syntax.ToReg8 pos $ Syntax.DL pos
_locToTarget pos Size8 (LocMem (RDX, offset)) = Syntax.ToMem8 pos (fromIntegral offset) $ Syntax.DL pos
_locToTarget pos Size8 (LocReg RDI) = Syntax.ToReg8 pos $ Syntax.DIL pos
_locToTarget pos Size8 (LocMem (RDI, offset)) = Syntax.ToMem8 pos (fromIntegral offset) $ Syntax.DIL pos
_locToTarget pos Size8 (LocReg RSI) = Syntax.ToReg8 pos $ Syntax.SIL pos
_locToTarget pos Size8 (LocMem (RSI, offset)) = Syntax.ToMem8 pos (fromIntegral offset) $ Syntax.SIL pos
_locToTarget pos Size8 (LocReg RSP) = Syntax.ToReg8 pos $ Syntax.SPL pos
_locToTarget pos Size8 (LocMem (RSP, offset)) = Syntax.ToMem8 pos (fromIntegral offset) $ Syntax.SPL pos
_locToTarget pos Size8 (LocReg RBP) = Syntax.ToReg8 pos $ Syntax.BPL pos
_locToTarget pos Size8 (LocMem (RBP, offset)) = Syntax.ToMem8 pos (fromIntegral offset) $ Syntax.BPL pos
_locToTarget pos Size8 (LocReg R8) = Syntax.ToReg8 pos $ Syntax.R8B pos
_locToTarget pos Size8 (LocMem (R8, offset)) = Syntax.ToMem8 pos (fromIntegral offset) $ Syntax.R8B pos
_locToTarget pos Size8 (LocReg R9) = Syntax.ToReg8 pos $ Syntax.R9B pos
_locToTarget pos Size8 (LocMem (R9, offset)) = Syntax.ToMem8 pos (fromIntegral offset) $ Syntax.R9B pos
_locToTarget pos Size8 (LocReg R10) = Syntax.ToReg8 pos $ Syntax.R10B pos
_locToTarget pos Size8 (LocMem (R10, offset)) = Syntax.ToMem8 pos (fromIntegral offset) $ Syntax.R10B pos
_locToTarget pos Size8 (LocReg R11) = Syntax.ToReg8 pos $ Syntax.R11B pos
_locToTarget pos Size8 (LocMem (R11, offset)) = Syntax.ToMem8 pos (fromIntegral offset) $ Syntax.R11B pos
_locToTarget pos Size8 (LocReg R12) = Syntax.ToReg8 pos $ Syntax.R12B pos
_locToTarget pos Size8 (LocMem (R12, offset)) = Syntax.ToMem8 pos (fromIntegral offset) $ Syntax.R12B pos
_locToTarget pos Size8 (LocReg R13) = Syntax.ToReg8 pos $ Syntax.R13B pos
_locToTarget pos Size8 (LocMem (R13, offset)) = Syntax.ToMem8 pos (fromIntegral offset) $ Syntax.R13B pos
_locToTarget pos Size8 (LocReg R14) = Syntax.ToReg8 pos $ Syntax.R14B pos
_locToTarget pos Size8 (LocMem (R14, offset)) = Syntax.ToMem8 pos (fromIntegral offset) $ Syntax.R14B pos
_locToTarget pos Size8 (LocReg R15) = Syntax.ToReg8 pos $ Syntax.R15B pos
_locToTarget pos Size8 (LocMem (R15, offset)) = Syntax.ToMem8 pos (fromIntegral offset) $ Syntax.R15B pos

-- Size classes

data Size = Size64 | Size32 | Size16 | Size8
	deriving (Eq, Ord, Show, Read, Generic, Typeable)


-- Instruction wrappers

data Instr a anno = ADD a Size (Loc) (Loc) (Annotation a anno) | AND a Size (Loc) (Loc) (Annotation a anno) | CMP a Size (Loc) (Loc) (Annotation a anno) | IDIV a Size (Loc) (Loc) (Annotation a anno) | IMUL a Size (Loc) (Loc) (Annotation a anno) | LEA a Size (Loc) (Loc) (Annotation a anno) | MOV a Size (Loc) (Loc) (Annotation a anno) | SUB a Size (Loc) (Loc) (Annotation a anno) | TEST a Size (Loc) (Loc) (Annotation a anno) | XOR a Size (Loc) (Loc) (Annotation a anno) | XCHG a Size (Loc) (Loc) (Annotation a anno) | SAL a Size (Loc) (Loc) (Annotation a anno) | SAR a Size (Loc) (Loc) (Annotation a anno) | NEG a Size (Loc) (Annotation a anno) | JMP a (String) (Annotation a anno) | JZ a (String) (Annotation a anno) | SETE a (Loc) (Annotation a anno) | SETG a (Loc) (Annotation a anno) | SETGE a (Loc) (Annotation a anno) | SETL a (Loc) (Annotation a anno) | SETLE a (Loc) (Annotation a anno) | SETNE a (Loc) (Annotation a anno) | POP a (Loc) (Annotation a anno) | PUSH a (Loc) (Annotation a anno) | LEAVE a (Annotation a anno) | RET a (Annotation a anno) | CDQ a (Annotation a anno)
	deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor, Typeable)



add :: (Monad m) => a -> Size -> Loc -> Loc -> ASMGeneratorT a anno m ()
add pos size loc1 loc2 =
	_emitInstr pos $ ADD pos size loc1 loc2 (NoAnnotation pos)


and :: (Monad m) => a -> Size -> Loc -> Loc -> ASMGeneratorT a anno m ()
and pos size loc1 loc2 =
	_emitInstr pos $ AND pos size loc1 loc2 (NoAnnotation pos)


cmp :: (Monad m) => a -> Size -> Loc -> Loc -> ASMGeneratorT a anno m ()
cmp pos size loc1 loc2 =
	_emitInstr pos $ CMP pos size loc1 loc2 (NoAnnotation pos)


idiv :: (Monad m) => a -> Size -> Loc -> Loc -> ASMGeneratorT a anno m ()
idiv pos size loc1 loc2 =
	_emitInstr pos $ IDIV pos size loc1 loc2 (NoAnnotation pos)


imul :: (Monad m) => a -> Size -> Loc -> Loc -> ASMGeneratorT a anno m ()
imul pos size loc1 loc2 =
	_emitInstr pos $ IMUL pos size loc1 loc2 (NoAnnotation pos)


lea :: (Monad m) => a -> Size -> Loc -> Loc -> ASMGeneratorT a anno m ()
lea pos size loc1 loc2 =
	_emitInstr pos $ LEA pos size loc1 loc2 (NoAnnotation pos)


mov :: (Monad m) => a -> Size -> Loc -> Loc -> ASMGeneratorT a anno m ()
mov pos size loc1 loc2 =
	_emitInstr pos $ MOV pos size loc1 loc2 (NoAnnotation pos)


sub :: (Monad m) => a -> Size -> Loc -> Loc -> ASMGeneratorT a anno m ()
sub pos size loc1 loc2 =
	_emitInstr pos $ SUB pos size loc1 loc2 (NoAnnotation pos)


test :: (Monad m) => a -> Size -> Loc -> Loc -> ASMGeneratorT a anno m ()
test pos size loc1 loc2 =
	_emitInstr pos $ TEST pos size loc1 loc2 (NoAnnotation pos)


xor :: (Monad m) => a -> Size -> Loc -> Loc -> ASMGeneratorT a anno m ()
xor pos size loc1 loc2 =
	_emitInstr pos $ XOR pos size loc1 loc2 (NoAnnotation pos)


xchg :: (Monad m) => a -> Size -> Loc -> Loc -> ASMGeneratorT a anno m ()
xchg pos size loc1 loc2 =
	_emitInstr pos $ XCHG pos size loc1 loc2 (NoAnnotation pos)


sal :: (Monad m) => a -> Size -> Loc -> Loc -> ASMGeneratorT a anno m ()
sal pos size loc1 loc2 =
	_emitInstr pos $ SAL pos size loc1 loc2 (NoAnnotation pos)


sar :: (Monad m) => a -> Size -> Loc -> Loc -> ASMGeneratorT a anno m ()
sar pos size loc1 loc2 =
	_emitInstr pos $ SAR pos size loc1 loc2 (NoAnnotation pos)


neg :: (Monad m) => a -> Size -> Loc -> ASMGeneratorT a anno m ()
neg pos size loc =
	_emitInstr pos $ NEG pos size loc (NoAnnotation pos)


sete :: (Monad m) => a -> Loc -> ASMGeneratorT a anno m ()
sete pos loc =
	_emitInstr pos $ SETE pos loc (NoAnnotation pos)


setg :: (Monad m) => a -> Loc -> ASMGeneratorT a anno m ()
setg pos loc =
	_emitInstr pos $ SETG pos loc (NoAnnotation pos)


setge :: (Monad m) => a -> Loc -> ASMGeneratorT a anno m ()
setge pos loc =
	_emitInstr pos $ SETGE pos loc (NoAnnotation pos)


setl :: (Monad m) => a -> Loc -> ASMGeneratorT a anno m ()
setl pos loc =
	_emitInstr pos $ SETL pos loc (NoAnnotation pos)


setle :: (Monad m) => a -> Loc -> ASMGeneratorT a anno m ()
setle pos loc =
	_emitInstr pos $ SETLE pos loc (NoAnnotation pos)


setne :: (Monad m) => a -> Loc -> ASMGeneratorT a anno m ()
setne pos loc =
	_emitInstr pos $ SETNE pos loc (NoAnnotation pos)


pop :: (Monad m) => a -> Loc -> ASMGeneratorT a anno m ()
pop pos loc =
	_emitInstr pos $ POP pos loc (NoAnnotation pos)


push :: (Monad m) => a -> Loc -> ASMGeneratorT a anno m ()
push pos loc =
	_emitInstr pos $ PUSH pos loc (NoAnnotation pos)


leave :: (Monad m) => a -> ASMGeneratorT a anno m ()
leave pos =
	_emitInstr pos $ LEAVE pos (NoAnnotation pos)


ret :: (Monad m) => a -> ASMGeneratorT a anno m ()
ret pos =
	_emitInstr pos $ RET pos (NoAnnotation pos)


cdq :: (Monad m) => a -> ASMGeneratorT a anno m ()
cdq pos =
	_emitInstr pos $ CDQ pos (NoAnnotation pos)


jmp :: (Monad m) => a -> String -> ASMGeneratorT a anno m ()
jmp pos label =
	_emitInstr pos $ JMP pos label (NoAnnotation pos)


jz :: (Monad m) => a -> String -> ASMGeneratorT a anno m ()
jz pos label =
	_emitInstr pos $ JZ pos label (NoAnnotation pos)

_convertInstr :: (Monad m) => Instr a anno -> ASMGeneratorT a anno m (Syntax.AsmInstr' a)
_convertInstr (ADD pos Size64 loc1 loc2 _) = return $ Syntax.ADD64 pos (_locToSource pos Size64 loc1) (_locToTarget pos Size64 loc2)
_convertInstr (ADD pos Size32 loc1 loc2 _) = return $ Syntax.ADD32 pos (_locToSource pos Size32 loc1) (_locToTarget pos Size32 loc2)
_convertInstr (ADD pos Size16 loc1 loc2 _) = return $ Syntax.ADD16 pos (_locToSource pos Size16 loc1) (_locToTarget pos Size16 loc2)
_convertInstr (ADD pos size _ _ _) = generatorFail $ EDataUnexpectedSize pos size
_convertInstr (AND pos Size64 loc1 loc2 _) = return $ Syntax.AND64 pos (_locToSource pos Size64 loc1) (_locToTarget pos Size64 loc2)
_convertInstr (AND pos Size32 loc1 loc2 _) = return $ Syntax.AND32 pos (_locToSource pos Size32 loc1) (_locToTarget pos Size32 loc2)
_convertInstr (AND pos Size16 loc1 loc2 _) = return $ Syntax.AND16 pos (_locToSource pos Size16 loc1) (_locToTarget pos Size16 loc2)
_convertInstr (AND pos size _ _ _) = generatorFail $ EDataUnexpectedSize pos size
_convertInstr (CMP pos Size64 loc1 loc2 _) = return $ Syntax.CMP64 pos (_locToSource pos Size64 loc1) (_locToTarget pos Size64 loc2)
_convertInstr (CMP pos Size32 loc1 loc2 _) = return $ Syntax.CMP32 pos (_locToSource pos Size32 loc1) (_locToTarget pos Size32 loc2)
_convertInstr (CMP pos Size16 loc1 loc2 _) = return $ Syntax.CMP16 pos (_locToSource pos Size16 loc1) (_locToTarget pos Size16 loc2)
_convertInstr (CMP pos size _ _ _) = generatorFail $ EDataUnexpectedSize pos size
_convertInstr (IDIV pos Size64 loc1 loc2 _) = return $ Syntax.IDIV64 pos (_locToSource pos Size64 loc1) (_locToTarget pos Size64 loc2)
_convertInstr (IDIV pos Size32 loc1 loc2 _) = return $ Syntax.IDIV32 pos (_locToSource pos Size32 loc1) (_locToTarget pos Size32 loc2)
_convertInstr (IDIV pos Size16 loc1 loc2 _) = return $ Syntax.IDIV16 pos (_locToSource pos Size16 loc1) (_locToTarget pos Size16 loc2)
_convertInstr (IDIV pos size _ _ _) = generatorFail $ EDataUnexpectedSize pos size
_convertInstr (IMUL pos Size64 loc1 loc2 _) = return $ Syntax.IMUL64 pos (_locToSource pos Size64 loc1) (_locToTarget pos Size64 loc2)
_convertInstr (IMUL pos Size32 loc1 loc2 _) = return $ Syntax.IMUL32 pos (_locToSource pos Size32 loc1) (_locToTarget pos Size32 loc2)
_convertInstr (IMUL pos Size16 loc1 loc2 _) = return $ Syntax.IMUL16 pos (_locToSource pos Size16 loc1) (_locToTarget pos Size16 loc2)
_convertInstr (IMUL pos size _ _ _) = generatorFail $ EDataUnexpectedSize pos size
_convertInstr (LEA pos Size64 loc1 loc2 _) = return $ Syntax.LEA64 pos (_locToSource pos Size64 loc1) (_locToTarget pos Size64 loc2)
_convertInstr (LEA pos Size32 loc1 loc2 _) = return $ Syntax.LEA32 pos (_locToSource pos Size32 loc1) (_locToTarget pos Size32 loc2)
_convertInstr (LEA pos Size16 loc1 loc2 _) = return $ Syntax.LEA16 pos (_locToSource pos Size16 loc1) (_locToTarget pos Size16 loc2)
_convertInstr (LEA pos size _ _ _) = generatorFail $ EDataUnexpectedSize pos size
_convertInstr (MOV pos Size64 loc1 loc2 _) = return $ Syntax.MOV64 pos (_locToSource pos Size64 loc1) (_locToTarget pos Size64 loc2)
_convertInstr (MOV pos Size32 loc1 loc2 _) = return $ Syntax.MOV32 pos (_locToSource pos Size32 loc1) (_locToTarget pos Size32 loc2)
_convertInstr (MOV pos Size16 loc1 loc2 _) = return $ Syntax.MOV16 pos (_locToSource pos Size16 loc1) (_locToTarget pos Size16 loc2)
_convertInstr (MOV pos size _ _ _) = generatorFail $ EDataUnexpectedSize pos size
_convertInstr (SUB pos Size64 loc1 loc2 _) = return $ Syntax.SUB64 pos (_locToSource pos Size64 loc1) (_locToTarget pos Size64 loc2)
_convertInstr (SUB pos Size32 loc1 loc2 _) = return $ Syntax.SUB32 pos (_locToSource pos Size32 loc1) (_locToTarget pos Size32 loc2)
_convertInstr (SUB pos Size16 loc1 loc2 _) = return $ Syntax.SUB16 pos (_locToSource pos Size16 loc1) (_locToTarget pos Size16 loc2)
_convertInstr (SUB pos size _ _ _) = generatorFail $ EDataUnexpectedSize pos size
_convertInstr (TEST pos Size64 loc1 loc2 _) = return $ Syntax.TEST64 pos (_locToSource pos Size64 loc1) (_locToTarget pos Size64 loc2)
_convertInstr (TEST pos Size32 loc1 loc2 _) = return $ Syntax.TEST32 pos (_locToSource pos Size32 loc1) (_locToTarget pos Size32 loc2)
_convertInstr (TEST pos Size16 loc1 loc2 _) = return $ Syntax.TEST16 pos (_locToSource pos Size16 loc1) (_locToTarget pos Size16 loc2)
_convertInstr (TEST pos size _ _ _) = generatorFail $ EDataUnexpectedSize pos size
_convertInstr (XOR pos Size64 loc1 loc2 _) = return $ Syntax.XOR64 pos (_locToSource pos Size64 loc1) (_locToTarget pos Size64 loc2)
_convertInstr (XOR pos Size32 loc1 loc2 _) = return $ Syntax.XOR32 pos (_locToSource pos Size32 loc1) (_locToTarget pos Size32 loc2)
_convertInstr (XOR pos Size16 loc1 loc2 _) = return $ Syntax.XOR16 pos (_locToSource pos Size16 loc1) (_locToTarget pos Size16 loc2)
_convertInstr (XOR pos size _ _ _) = generatorFail $ EDataUnexpectedSize pos size
_convertInstr (XCHG pos Size64 loc1 loc2 _) = return $ Syntax.XCHG64 pos (_locToSource pos Size64 loc1) (_locToTarget pos Size64 loc2)
_convertInstr (XCHG pos Size32 loc1 loc2 _) = return $ Syntax.XCHG32 pos (_locToSource pos Size32 loc1) (_locToTarget pos Size32 loc2)
_convertInstr (XCHG pos Size16 loc1 loc2 _) = return $ Syntax.XCHG16 pos (_locToSource pos Size16 loc1) (_locToTarget pos Size16 loc2)
_convertInstr (XCHG pos size _ _ _) = generatorFail $ EDataUnexpectedSize pos size
_convertInstr (SAL pos Size64 loc1 loc2 _) = return $ Syntax.SAL64 pos (_locToSource pos Size64 loc1) (_locToTarget pos Size64 loc2)
_convertInstr (SAL pos Size32 loc1 loc2 _) = return $ Syntax.SAL32 pos (_locToSource pos Size32 loc1) (_locToTarget pos Size32 loc2)
_convertInstr (SAL pos Size16 loc1 loc2 _) = return $ Syntax.SAL16 pos (_locToSource pos Size16 loc1) (_locToTarget pos Size16 loc2)
_convertInstr (SAL pos size _ _ _) = generatorFail $ EDataUnexpectedSize pos size
_convertInstr (SAR pos Size64 loc1 loc2 _) = return $ Syntax.SAR64 pos (_locToSource pos Size64 loc1) (_locToTarget pos Size64 loc2)
_convertInstr (SAR pos Size32 loc1 loc2 _) = return $ Syntax.SAR32 pos (_locToSource pos Size32 loc1) (_locToTarget pos Size32 loc2)
_convertInstr (SAR pos Size16 loc1 loc2 _) = return $ Syntax.SAR16 pos (_locToSource pos Size16 loc1) (_locToTarget pos Size16 loc2)
_convertInstr (SAR pos size _ _ _) = generatorFail $ EDataUnexpectedSize pos size
_convertInstr (NEG pos Size64 loc _) = return $ Syntax.NEG64 pos (_locToTarget pos Size64 loc)
_convertInstr (NEG pos Size32 loc _) = return $ Syntax.NEG32 pos (_locToTarget pos Size32 loc)
_convertInstr (NEG pos Size16 loc _) = return $ Syntax.NEG16 pos (_locToTarget pos Size16 loc)
_convertInstr (NEG pos size _ _) = generatorFail $ EDataUnexpectedSize pos size
_convertInstr (SETE pos loc _) = let (Syntax.ToReg8 _ r) = (_locToTarget pos Size8 loc) in return $ Syntax.SETE pos r
_convertInstr (SETE pos loc _) = generatorFail $ ENonRegisterLocationGiven pos loc
_convertInstr (SETG pos loc _) = let (Syntax.ToReg8 _ r) = (_locToTarget pos Size8 loc) in return $ Syntax.SETG pos r
_convertInstr (SETG pos loc _) = generatorFail $ ENonRegisterLocationGiven pos loc
_convertInstr (SETGE pos loc _) = let (Syntax.ToReg8 _ r) = (_locToTarget pos Size8 loc) in return $ Syntax.SETGE pos r
_convertInstr (SETGE pos loc _) = generatorFail $ ENonRegisterLocationGiven pos loc
_convertInstr (SETL pos loc _) = let (Syntax.ToReg8 _ r) = (_locToTarget pos Size8 loc) in return $ Syntax.SETL pos r
_convertInstr (SETL pos loc _) = generatorFail $ ENonRegisterLocationGiven pos loc
_convertInstr (SETLE pos loc _) = let (Syntax.ToReg8 _ r) = (_locToTarget pos Size8 loc) in return $ Syntax.SETLE pos r
_convertInstr (SETLE pos loc _) = generatorFail $ ENonRegisterLocationGiven pos loc
_convertInstr (SETNE pos loc _) = let (Syntax.ToReg8 _ r) = (_locToTarget pos Size8 loc) in return $ Syntax.SETNE pos r
_convertInstr (SETNE pos loc _) = generatorFail $ ENonRegisterLocationGiven pos loc
_convertInstr (POP pos loc@(LocReg reg) _) = let (Syntax.ToReg64 _ r) = (_locToTarget pos Size64 loc) in return $ Syntax.POP pos r
_convertInstr (POP pos loc _) = generatorFail $ ENonRegisterLocationGiven pos loc
_convertInstr (PUSH pos loc@(LocReg reg) _) = let (Syntax.ToReg64 _ r) = (_locToTarget pos Size64 loc) in return $ Syntax.PUSH pos r
_convertInstr (PUSH pos loc _) = generatorFail $ ENonRegisterLocationGiven pos loc
_convertInstr (JMP pos label _) = return $ Syntax.JMP pos (Syntax.Label label)
_convertInstr (JZ pos label _) = return $ Syntax.JZ pos (Syntax.Label label)
_convertInstr (LEAVE pos _) = return $ Syntax.LEAVE pos
_convertInstr (RET pos _) = return $ Syntax.RET pos
_convertInstr (CDQ pos _) = return $ Syntax.CDQ pos

-- Data wrappers
dataDef :: a -> DataDef -> ASMGenerator a anno ()
dataDef pos def = _emitDef pos $ convertDataDef pos def
