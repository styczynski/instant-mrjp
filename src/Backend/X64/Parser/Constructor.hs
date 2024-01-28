
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
module Backend.X64.Parser.Constructor(runASMGeneratorT
, execASMGeneratorT
, continueASMGeneratorT
, ASMGenerator
, ASMGeneratorT
, dataDef
, bssDef
, DataDef(..)
, Data(..)
, Loc(..)
, Size(..)
, Annotation(..)
, mapInstrData
, getInstrUsedLabels
, mapAnnotationData
, ValOrd(..)
, negValOrd
, combineAnn
, CommentProvider(..)
, Reg(..)
, Loc(..)
, RegType(..)
, isReg
, asReg
, argLoc
, allRegs
, regType
, asLoc
, showReg
, Instr(..)
, label
, add
, and
, cmp
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
, idiv
, inc
, dec
, push
, sete
, setg
, setge
, setl
, setle
, setne
, setz
, setnz
, pop
, leave
, ret
, cdq
, jmp
, je
, jg
, jge
, jl
, jle
, jne
, jz
, jnz
, call
, callIndirect
, LocFunctor(..)
, toBytes) where

import Data.Generics.Product
import Data.Generics.Sum
import GHC.Generics (Generic)
import Data.Typeable
import qualified Data.Data as D

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer.Lazy

import qualified Data.Text as T

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
	| Data8I Integer
	| Data64From String
	| Data32From String
	| Data8From String
	deriving (Eq, Ord, Show, Read, D.Data, Typeable, Generic)

convertDataDef :: a -> DataDef -> Syntax.AsmDataDef' a
convertDataDef pos (DataGlobal label) = Syntax.AsmDataGlobal pos $ Syntax.Label $ sanitizeLabel label
convertDataDef pos (DataDef label datas) = Syntax.AsmDataDef pos (Syntax.Label $ sanitizeLabel label) (map (convertData pos) datas)
	where
		convertData :: a -> Data -> Syntax.Data' a
		convertData pos (DataStr str) = Syntax.DataString pos $ str
		convertData pos (Data64I val) = Syntax.Data64 pos $ Syntax.ConstInt pos $ val
		convertData pos (Data32I val) = Syntax.Data32 pos $ Syntax.ConstInt pos $ val
		convertData pos (Data8I val) = Syntax.Data8 pos $ Syntax.ConstInt pos $ val
		convertData pos (Data64From label) = Syntax.Data64 pos $ Syntax.ConstLabel pos $ Syntax.Label $ sanitizeLabel label
		convertData pos (Data32From label) = Syntax.Data32 pos $ Syntax.ConstLabel pos $ Syntax.Label $ sanitizeLabel label
		convertData pos (Data8From label) = Syntax.Data8 pos $ Syntax.ConstLabel pos $ Syntax.Label $ sanitizeLabel label


-- Error definition
data GeneratorError a =
	EDataMemToMemTransfer a Loc Loc
	| EDataUnexpectedSize a Size
	| EDataUnexpectedLocation a Loc
	| ENoOutputCodeGenerated
	| ENonRegisterLocationGiven a String Loc
	deriving (Eq, Ord, Show, Read, Generic, Typeable)

generatorFail :: (Monad m) => GeneratorError a -> ASMGeneratorT a anno m v
generatorFail e = throwError e

-- Assembly generator definition
type ASMGenerator a anno v = (WriterT (GeneratorOut a anno) (Except (GeneratorError a))) v
type ASMGeneratorT a anno (m :: * -> *) = (WriterT (GeneratorOut a anno) (ExceptT (GeneratorError a) m))

data GeneratorOut a anno = GeneratorOut (Maybe a) [Instr a anno] [Syntax.AsmDataDef' a] [Syntax.AsmDataDef' a]

instance Semigroup (GeneratorOut a anno) where
	(<>) (GeneratorOut Nothing instr1 defs1 bss1) (GeneratorOut (Just firstPos2) instr2 defs2 bss2) = GeneratorOut (Just firstPos2) (instr1 <> instr2) (defs1 <> defs2) (bss1 <> bss2)
	(<>) (GeneratorOut (Just firstPos1) instr1 defs1 bss1) (GeneratorOut Nothing instr2 defs2 bss2) = GeneratorOut (Just firstPos1) (instr1 <> instr2) (defs1 <> defs2) (bss1 <> bss2)
	(<>) (GeneratorOut firstPos1 instr1 defs1 bss1) (GeneratorOut _ instr2 defs2 bss2) = GeneratorOut firstPos1 (instr1 <> instr2) (defs1 <> defs2) (bss1 <> bss2)

instance Monoid (GeneratorOut a anno) where
	mempty = GeneratorOut Nothing [] [] []

_emitInstr :: (Monad m) => a -> Instr a anno -> ASMGeneratorT a anno m ()
_emitInstr pos instr = tell $ GeneratorOut (Just pos) [instr] [] []

_emitDef :: (Monad m) => a -> Syntax.AsmDataDef' a -> ASMGeneratorT a anno m ()
_emitDef pos def = tell $ GeneratorOut (Just pos) [] [def] []

_emitBSS :: (Monad m) => a -> Syntax.AsmDataDef' a -> ASMGeneratorT a anno m ()
_emitBSS pos def = tell $ GeneratorOut (Just pos) [] [] [def]

continueASMGeneratorT :: (Monad m) => GeneratorOut a anno -> ASMGeneratorT a anno m ()
continueASMGeneratorT out = tell out

execASMGeneratorT :: (Monad m) => (ASMGeneratorT a anno m v) -> m (Either (GeneratorError a) (v, GeneratorOut a anno))
execASMGeneratorT generator = do
	genResult <- runExceptT (runWriterT $ generator)
	return genResult

runASMGeneratorT :: (Monad m, CommentProvider a anno) => (ASMGeneratorT a anno m v) -> [String] -> ([Instr a anno] -> m [Instr a anno]) -> m (Either (GeneratorError a) (String, v))
runASMGeneratorT generator externs transformFn = do
	genResult <- runExceptT (runWriterT $ generator)
	case genResult of
		Left err -> return $ Left err
		Right (result, _) -> do
			outResult <- runExceptT (runWriterT $ generateOut externs genResult transformFn)
			case outResult  of
				Left err -> return $ Left err
				Right ((_, fullProg), _) -> do
					let assemblyCodeStr = Printer.printTree fullProg
					let codeLines = map (T.strip) $ T.lines $ T.replace "<ENDL>" "\n" $ T.pack assemblyCodeStr
					let formattedCode = alignASMCommentsText codeLines
					return $ Right (formattedCode, result)
	where
		generateOut :: (Monad m, CommentProvider a anno) => [String] -> Either (GeneratorError a) (v, GeneratorOut a anno) -> ([Instr a anno] -> m [Instr a anno]) -> (ASMGeneratorT a anno m (v, Syntax.AsmProgram' a))
		generateOut externs r transformFn  =
			case r of
				Left err -> generatorFail err
				Right (_, GeneratorOut Nothing _ _ _) -> generatorFail ENoOutputCodeGenerated
				Right (result, GeneratorOut (Just pos) instrs defs bss) -> do
					instrs' <- lift $ lift $  transformFn instrs
					instrs'' <- mapM _convertInstr instrs'
					let topDirectives = map (Syntax.Extern pos . Syntax.Label . sanitizeLabel) externs
					let fullProg = Syntax.AsmProgram pos topDirectives (Syntax.SectionData pos defs) (Syntax.SectionBSS pos bss) (Syntax.SectionCode pos instrs'')
					return (result, fullProg)
		alignASMCommentsText :: [T.Text] -> String
		alignASMCommentsText fileContents =
			let codeLines = map (\line -> let chunks = T.splitOn (T.pack "#") line in if null chunks then (T.pack "", T.pack "") else (head chunks, T.intercalate "#" $ tail chunks)) $ fileContents in
			let codeWidth = maximum $ map (\(code, _) -> T.length code) codeLines in
			let formattedCodeLines = map (\(code, comment) -> let c' = paddingTo code codeWidth in if T.null comment then code else c' <> (T.pack " #") <> comment) codeLines in
			T.unpack $ T.unlines formattedCodeLines
		paddingTo :: T.Text -> Int -> T.Text
		paddingTo line width = line <> (T.pack (concat $ replicate (width - (T.length line)) " "))


-- Registers

data Reg = RAX| RBX| RCX| RDX| RDI| RSI| RSP| RBP| R8| R9| R10| R11| R12| R13| R14| R15
	deriving (Eq, Ord, Show, Read, Generic, Typeable)

data RegType = CallerSaved | CalleeSaved deriving (Eq, Show)

-- Caller saved registers are preferred over callee saved.
instance Ord RegType where
	compare rt1 rt2 = case (rt1, rt2) of
		(CallerSaved, CallerSaved) -> EQ
		(CalleeSaved, CalleeSaved) -> EQ
		(CalleeSaved, CallerSaved) -> LT
		(CallerSaved, CalleeSaved) -> GT

allRegs :: [Reg]
allRegs = [RAX, RBX, RCX, RDX, RDI, RSI, R8, R9, R10, R11, R12, R13, R14, R15]

asLoc :: Reg -> Loc
asLoc reg = LocReg reg

sanitizeLabel :: String -> String
sanitizeLabel s = case s of
	[]     -> []
	'~':xs -> '_':'_':sanitizeLabel xs
	x:xs   -> x:sanitizeLabel xs


regType :: Reg -> RegType
regType RAX = CallerSaved
regType RBX = CalleeSaved
regType RCX = CallerSaved
regType RDX = CallerSaved
regType RDI = CallerSaved
regType RSI = CallerSaved
regType RSP = CallerSaved
regType RBP = CalleeSaved
regType R8 = CallerSaved
regType R9 = CallerSaved
regType R10 = CallerSaved
regType R11 = CallerSaved
regType R12 = CalleeSaved
regType R13 = CalleeSaved
regType R14 = CalleeSaved
regType R15 = CalleeSaved
showReg :: Size -> Reg -> String
showReg Size64 RAX = "RAX"
showReg Size64 RBX = "RBX"
showReg Size64 RCX = "RCX"
showReg Size64 RDX = "RDX"
showReg Size64 RDI = "RDI"
showReg Size64 RSI = "RSI"
showReg Size64 RSP = "RSP"
showReg Size64 RBP = "RBP"
showReg Size64 R8 = "R8"
showReg Size64 R9 = "R9"
showReg Size64 R10 = "R10"
showReg Size64 R11 = "R11"
showReg Size64 R12 = "R12"
showReg Size64 R13 = "R13"
showReg Size64 R14 = "R14"
showReg Size64 R15 = "R15"
showReg Size32 RAX = "EAX"
showReg Size32 RBX = "EBX"
showReg Size32 RCX = "ECX"
showReg Size32 RDX = "EDX"
showReg Size32 RDI = "EDI"
showReg Size32 RSI = "ESI"
showReg Size32 RSP = "ESP"
showReg Size32 RBP = "EBP"
showReg Size32 R8 = "R8D"
showReg Size32 R9 = "R9D"
showReg Size32 R10 = "R10D"
showReg Size32 R11 = "R11D"
showReg Size32 R12 = "R12D"
showReg Size32 R13 = "R13D"
showReg Size32 R14 = "R14D"
showReg Size32 R15 = "R15D"
showReg Size16 RAX = "AX"
showReg Size16 RBX = "BX"
showReg Size16 RCX = "CX"
showReg Size16 RDX = "DX"
showReg Size16 RDI = "DI"
showReg Size16 RSI = "SI"
showReg Size16 RSP = "SP"
showReg Size16 RBP = "BP"
showReg Size16 R8 = "R8W"
showReg Size16 R9 = "R9W"
showReg Size16 R10 = "R10W"
showReg Size16 R11 = "R11W"
showReg Size16 R12 = "R12W"
showReg Size16 R13 = "R13W"
showReg Size16 R14 = "R14W"
showReg Size16 R15 = "R15W"
showReg Size8 RAX = "AL"
showReg Size8 RBX = "BL"
showReg Size8 RCX = "CL"
showReg Size8 RDX = "DL"
showReg Size8 RDI = "DIL"
showReg Size8 RSI = "SIL"
showReg Size8 RSP = "SPL"
showReg Size8 RBP = "BPL"
showReg Size8 R8 = "R8B"
showReg Size8 R9 = "R9B"
showReg Size8 R10 = "R10B"
showReg Size8 R11 = "R11B"
showReg Size8 R12 = "R12B"
showReg Size8 R13 = "R13B"
showReg Size8 R14 = "R14B"
showReg Size8 R15 = "R15B"


data ValOrd = OrdE | OrdG | OrdGE | OrdL | OrdLE | OrdNE | OrdZ | OrdNZ
	deriving (Eq, Ord, Read, Generic, Typeable)

instance Show ValOrd where
	show OrdE = "=="
	show OrdG = ">"
	show OrdGE = ">="
	show OrdL = "<"
	show OrdLE = "<="
	show OrdNE = "/="
	show OrdZ = "=="
	show OrdNZ = "/="

negValOrd :: ValOrd -> ValOrd
negValOrd OrdE = OrdNE
negValOrd OrdG = OrdLE
negValOrd OrdGE = OrdL
negValOrd OrdL = OrdGE
negValOrd OrdLE = OrdG
negValOrd OrdNE = OrdE
negValOrd OrdZ = OrdNZ
negValOrd OrdNZ = OrdZ

data Loc = LocLabel String | LocLabelPIC String | LocConst Integer | LocReg Reg | LocMem (Reg, Int64) | LocMemOffset { ptrBase :: Reg, ptrIdx :: Reg, ptrOffset :: Int64, ptrScale :: Size }
	deriving (Eq, Ord, Show, Read, Generic, Typeable)

class LocFunctor a where
	mapLoc :: (Loc -> Loc) -> a -> a

instance LocFunctor Loc where
	mapLoc fn = fn

-- instance (Functor f, LocFunctor g) => LocFunctor (f g) where
-- 	mapLoc fn = fmap (mapLoc fn)

instance (LocFunctor g) => LocFunctor [g] where
	mapLoc fn = fmap (mapLoc fn)

instance LocFunctor (GeneratorOut a anno) where
	mapLoc fn (GeneratorOut pos instrs defs bss) = GeneratorOut pos (mapLoc fn instrs) defs bss

isReg :: Loc -> Bool
isReg loc = case loc of
	LocReg _ -> True
	_        -> False

asReg :: Loc -> Reg
asReg loc = case loc of
	LocReg r -> r
	_        -> error "asReg: not a reg"

data Annotation a anno = NoAnnotation a | Annotation a anno
	deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor, Typeable)

mapAnnotationData :: (a -> b) -> (anno1 -> anno2) -> Annotation a anno1 -> Annotation b anno2
mapAnnotationData f g (NoAnnotation p) = NoAnnotation (f p)
mapAnnotationData f g (Annotation p ann) = Annotation (f p) (g ann)

combineAnn :: Annotation a anno -> Annotation a anno -> Annotation a anno
combineAnn ann1@(Annotation a anno) _ = ann1
combineAnn (NoAnnotation _) ann2 = ann2

class CommentProvider a anno | a -> anno where
	toComment :: a -> anno -> String
	defaultComment :: a -> Maybe String
	defaultComment _ = Nothing

_convert_annotation :: (CommentProvider a anno) => Annotation a anno -> Syntax.CommentAnn' a
_convert_annotation (NoAnnotation pos) = case (defaultComment pos) of
	Nothing -> Syntax.NoComment pos
	(Just comment) -> Syntax.Comment pos $ Syntax.CommentLike $ "#-- " ++ comment ++ " --#"
_convert_annotation (Annotation pos ann) = Syntax.Comment pos $ Syntax.CommentLike $ "#-- " ++ (toComment pos ann) ++ " --#"


argLoc :: Integer -> Loc
argLoc 0 = LocReg RDI
argLoc 1 = LocReg RSI
argLoc 2 = LocReg RDX
argLoc 3 = LocReg RCX
argLoc 4 = LocReg R8
argLoc 5 = LocReg R9
argLoc argIndex = LocMem (RBP, (fromInteger argIndex - 6) * 8 + 8)

_locToSource :: a -> Size -> Loc -> Syntax.Source' a
_locToSource pos _ (LocConst val) = Syntax.FromConst pos $ Syntax.ConstIntRef $ "$" ++ show val
_locToSource pos Size64 (LocLabel l) = Syntax.FromLabel64 pos $ Syntax.Label $ sanitizeLabel l
_locToSource pos Size64 (LocLabelPIC l) = Syntax.FromLabelOffset64 pos $ Syntax.Label $ sanitizeLabel l
_locToSource pos Size64 (LocMemOffset ptrBase ptrIdx ptrOffset ptrScale) =
	let (Syntax.FromMem64 _ _ rBase) = _locToSource pos Size64 (LocMem (ptrBase, 0)) in
	let (Syntax.FromMem64 _ _ rIdx) =  _locToSource pos Size64 (LocMem (ptrIdx, 0)) in
	Syntax.FromMemComplex64 pos (fromIntegral ptrOffset) rBase rIdx (fromIntegral $ toBytes $ ptrScale)
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
_locToSource pos Size32 (LocLabel l) = Syntax.FromLabel32 pos $ Syntax.Label $ sanitizeLabel l
_locToSource pos Size32 (LocLabelPIC l) = Syntax.FromLabelOffset32 pos $ Syntax.Label $ sanitizeLabel l
_locToSource pos Size32 (LocMemOffset ptrBase ptrIdx ptrOffset ptrScale) =
	let (Syntax.FromMem32 _ _ rBase) = _locToSource pos Size32 (LocMem (ptrBase, 0)) in
	let (Syntax.FromMem32 _ _ rIdx) =  _locToSource pos Size32 (LocMem (ptrIdx, 0)) in
	Syntax.FromMemComplex32 pos (fromIntegral ptrOffset) rBase rIdx (fromIntegral $ toBytes $ ptrScale)
_locToSource pos Size32 (LocReg RAX) = Syntax.FromReg32 pos $ Syntax.EAX pos
_locToSource pos Size32 (LocMem (RAX, offset)) = Syntax.FromMem32 pos (fromIntegral offset) $ Syntax.RAX pos
_locToSource pos Size32 (LocReg RBX) = Syntax.FromReg32 pos $ Syntax.EBX pos
_locToSource pos Size32 (LocMem (RBX, offset)) = Syntax.FromMem32 pos (fromIntegral offset) $ Syntax.RBX pos
_locToSource pos Size32 (LocReg RCX) = Syntax.FromReg32 pos $ Syntax.ECX pos
_locToSource pos Size32 (LocMem (RCX, offset)) = Syntax.FromMem32 pos (fromIntegral offset) $ Syntax.RCX pos
_locToSource pos Size32 (LocReg RDX) = Syntax.FromReg32 pos $ Syntax.EDX pos
_locToSource pos Size32 (LocMem (RDX, offset)) = Syntax.FromMem32 pos (fromIntegral offset) $ Syntax.RDX pos
_locToSource pos Size32 (LocReg RDI) = Syntax.FromReg32 pos $ Syntax.EDI pos
_locToSource pos Size32 (LocMem (RDI, offset)) = Syntax.FromMem32 pos (fromIntegral offset) $ Syntax.RDI pos
_locToSource pos Size32 (LocReg RSI) = Syntax.FromReg32 pos $ Syntax.ESI pos
_locToSource pos Size32 (LocMem (RSI, offset)) = Syntax.FromMem32 pos (fromIntegral offset) $ Syntax.RSI pos
_locToSource pos Size32 (LocReg RSP) = Syntax.FromReg32 pos $ Syntax.ESP pos
_locToSource pos Size32 (LocMem (RSP, offset)) = Syntax.FromMem32 pos (fromIntegral offset) $ Syntax.RSP pos
_locToSource pos Size32 (LocReg RBP) = Syntax.FromReg32 pos $ Syntax.EBP pos
_locToSource pos Size32 (LocMem (RBP, offset)) = Syntax.FromMem32 pos (fromIntegral offset) $ Syntax.RBP pos
_locToSource pos Size32 (LocReg R8) = Syntax.FromReg32 pos $ Syntax.R8D pos
_locToSource pos Size32 (LocMem (R8, offset)) = Syntax.FromMem32 pos (fromIntegral offset) $ Syntax.R8 pos
_locToSource pos Size32 (LocReg R9) = Syntax.FromReg32 pos $ Syntax.R9D pos
_locToSource pos Size32 (LocMem (R9, offset)) = Syntax.FromMem32 pos (fromIntegral offset) $ Syntax.R9 pos
_locToSource pos Size32 (LocReg R10) = Syntax.FromReg32 pos $ Syntax.R10D pos
_locToSource pos Size32 (LocMem (R10, offset)) = Syntax.FromMem32 pos (fromIntegral offset) $ Syntax.R10 pos
_locToSource pos Size32 (LocReg R11) = Syntax.FromReg32 pos $ Syntax.R11D pos
_locToSource pos Size32 (LocMem (R11, offset)) = Syntax.FromMem32 pos (fromIntegral offset) $ Syntax.R11 pos
_locToSource pos Size32 (LocReg R12) = Syntax.FromReg32 pos $ Syntax.R12D pos
_locToSource pos Size32 (LocMem (R12, offset)) = Syntax.FromMem32 pos (fromIntegral offset) $ Syntax.R12 pos
_locToSource pos Size32 (LocReg R13) = Syntax.FromReg32 pos $ Syntax.R13D pos
_locToSource pos Size32 (LocMem (R13, offset)) = Syntax.FromMem32 pos (fromIntegral offset) $ Syntax.R13 pos
_locToSource pos Size32 (LocReg R14) = Syntax.FromReg32 pos $ Syntax.R14D pos
_locToSource pos Size32 (LocMem (R14, offset)) = Syntax.FromMem32 pos (fromIntegral offset) $ Syntax.R14 pos
_locToSource pos Size32 (LocReg R15) = Syntax.FromReg32 pos $ Syntax.R15D pos
_locToSource pos Size32 (LocMem (R15, offset)) = Syntax.FromMem32 pos (fromIntegral offset) $ Syntax.R15 pos
_locToSource pos Size16 (LocLabel l) = Syntax.FromLabel16 pos $ Syntax.Label $ sanitizeLabel l
_locToSource pos Size16 (LocLabelPIC l) = Syntax.FromLabelOffset16 pos $ Syntax.Label $ sanitizeLabel l
_locToSource pos Size16 (LocMemOffset ptrBase ptrIdx ptrOffset ptrScale) =
	let (Syntax.FromMem16 _ _ rBase) = _locToSource pos Size16 (LocMem (ptrBase, 0)) in
	let (Syntax.FromMem16 _ _ rIdx) =  _locToSource pos Size16 (LocMem (ptrIdx, 0)) in
	Syntax.FromMemComplex16 pos (fromIntegral ptrOffset) rBase rIdx (fromIntegral $ toBytes $ ptrScale)
_locToSource pos Size16 (LocReg RAX) = Syntax.FromReg16 pos $ Syntax.AX pos
_locToSource pos Size16 (LocMem (RAX, offset)) = Syntax.FromMem16 pos (fromIntegral offset) $ Syntax.RAX pos
_locToSource pos Size16 (LocReg RBX) = Syntax.FromReg16 pos $ Syntax.BX pos
_locToSource pos Size16 (LocMem (RBX, offset)) = Syntax.FromMem16 pos (fromIntegral offset) $ Syntax.RBX pos
_locToSource pos Size16 (LocReg RCX) = Syntax.FromReg16 pos $ Syntax.CX pos
_locToSource pos Size16 (LocMem (RCX, offset)) = Syntax.FromMem16 pos (fromIntegral offset) $ Syntax.RCX pos
_locToSource pos Size16 (LocReg RDX) = Syntax.FromReg16 pos $ Syntax.DX pos
_locToSource pos Size16 (LocMem (RDX, offset)) = Syntax.FromMem16 pos (fromIntegral offset) $ Syntax.RDX pos
_locToSource pos Size16 (LocReg RDI) = Syntax.FromReg16 pos $ Syntax.DI pos
_locToSource pos Size16 (LocMem (RDI, offset)) = Syntax.FromMem16 pos (fromIntegral offset) $ Syntax.RDI pos
_locToSource pos Size16 (LocReg RSI) = Syntax.FromReg16 pos $ Syntax.SI pos
_locToSource pos Size16 (LocMem (RSI, offset)) = Syntax.FromMem16 pos (fromIntegral offset) $ Syntax.RSI pos
_locToSource pos Size16 (LocReg RSP) = Syntax.FromReg16 pos $ Syntax.SP pos
_locToSource pos Size16 (LocMem (RSP, offset)) = Syntax.FromMem16 pos (fromIntegral offset) $ Syntax.RSP pos
_locToSource pos Size16 (LocReg RBP) = Syntax.FromReg16 pos $ Syntax.BP pos
_locToSource pos Size16 (LocMem (RBP, offset)) = Syntax.FromMem16 pos (fromIntegral offset) $ Syntax.RBP pos
_locToSource pos Size16 (LocReg R8) = Syntax.FromReg16 pos $ Syntax.R8W pos
_locToSource pos Size16 (LocMem (R8, offset)) = Syntax.FromMem16 pos (fromIntegral offset) $ Syntax.R8 pos
_locToSource pos Size16 (LocReg R9) = Syntax.FromReg16 pos $ Syntax.R9W pos
_locToSource pos Size16 (LocMem (R9, offset)) = Syntax.FromMem16 pos (fromIntegral offset) $ Syntax.R9 pos
_locToSource pos Size16 (LocReg R10) = Syntax.FromReg16 pos $ Syntax.R10W pos
_locToSource pos Size16 (LocMem (R10, offset)) = Syntax.FromMem16 pos (fromIntegral offset) $ Syntax.R10 pos
_locToSource pos Size16 (LocReg R11) = Syntax.FromReg16 pos $ Syntax.R11W pos
_locToSource pos Size16 (LocMem (R11, offset)) = Syntax.FromMem16 pos (fromIntegral offset) $ Syntax.R11 pos
_locToSource pos Size16 (LocReg R12) = Syntax.FromReg16 pos $ Syntax.R12W pos
_locToSource pos Size16 (LocMem (R12, offset)) = Syntax.FromMem16 pos (fromIntegral offset) $ Syntax.R12 pos
_locToSource pos Size16 (LocReg R13) = Syntax.FromReg16 pos $ Syntax.R13W pos
_locToSource pos Size16 (LocMem (R13, offset)) = Syntax.FromMem16 pos (fromIntegral offset) $ Syntax.R13 pos
_locToSource pos Size16 (LocReg R14) = Syntax.FromReg16 pos $ Syntax.R14W pos
_locToSource pos Size16 (LocMem (R14, offset)) = Syntax.FromMem16 pos (fromIntegral offset) $ Syntax.R14 pos
_locToSource pos Size16 (LocReg R15) = Syntax.FromReg16 pos $ Syntax.R15W pos
_locToSource pos Size16 (LocMem (R15, offset)) = Syntax.FromMem16 pos (fromIntegral offset) $ Syntax.R15 pos
_locToSource pos Size8 (LocLabel l) = Syntax.FromLabel8 pos $ Syntax.Label $ sanitizeLabel l
_locToSource pos Size8 (LocLabelPIC l) = Syntax.FromLabelOffset8 pos $ Syntax.Label $ sanitizeLabel l
_locToSource pos Size8 (LocMemOffset ptrBase ptrIdx ptrOffset ptrScale) =
	let (Syntax.FromMem8 _ _ rBase) = _locToSource pos Size8 (LocMem (ptrBase, 0)) in
	let (Syntax.FromMem8 _ _ rIdx) =  _locToSource pos Size8 (LocMem (ptrIdx, 0)) in
	Syntax.FromMemComplex8 pos (fromIntegral ptrOffset) rBase rIdx (fromIntegral $ toBytes $ ptrScale)
_locToSource pos Size8 (LocReg RAX) = Syntax.FromReg8 pos $ Syntax.AL pos
_locToSource pos Size8 (LocMem (RAX, offset)) = Syntax.FromMem8 pos (fromIntegral offset) $ Syntax.RAX pos
_locToSource pos Size8 (LocReg RBX) = Syntax.FromReg8 pos $ Syntax.BL pos
_locToSource pos Size8 (LocMem (RBX, offset)) = Syntax.FromMem8 pos (fromIntegral offset) $ Syntax.RBX pos
_locToSource pos Size8 (LocReg RCX) = Syntax.FromReg8 pos $ Syntax.CL pos
_locToSource pos Size8 (LocMem (RCX, offset)) = Syntax.FromMem8 pos (fromIntegral offset) $ Syntax.RCX pos
_locToSource pos Size8 (LocReg RDX) = Syntax.FromReg8 pos $ Syntax.DL pos
_locToSource pos Size8 (LocMem (RDX, offset)) = Syntax.FromMem8 pos (fromIntegral offset) $ Syntax.RDX pos
_locToSource pos Size8 (LocReg RDI) = Syntax.FromReg8 pos $ Syntax.DIL pos
_locToSource pos Size8 (LocMem (RDI, offset)) = Syntax.FromMem8 pos (fromIntegral offset) $ Syntax.RDI pos
_locToSource pos Size8 (LocReg RSI) = Syntax.FromReg8 pos $ Syntax.SIL pos
_locToSource pos Size8 (LocMem (RSI, offset)) = Syntax.FromMem8 pos (fromIntegral offset) $ Syntax.RSI pos
_locToSource pos Size8 (LocReg RSP) = Syntax.FromReg8 pos $ Syntax.SPL pos
_locToSource pos Size8 (LocMem (RSP, offset)) = Syntax.FromMem8 pos (fromIntegral offset) $ Syntax.RSP pos
_locToSource pos Size8 (LocReg RBP) = Syntax.FromReg8 pos $ Syntax.BPL pos
_locToSource pos Size8 (LocMem (RBP, offset)) = Syntax.FromMem8 pos (fromIntegral offset) $ Syntax.RBP pos
_locToSource pos Size8 (LocReg R8) = Syntax.FromReg8 pos $ Syntax.R8B pos
_locToSource pos Size8 (LocMem (R8, offset)) = Syntax.FromMem8 pos (fromIntegral offset) $ Syntax.R8 pos
_locToSource pos Size8 (LocReg R9) = Syntax.FromReg8 pos $ Syntax.R9B pos
_locToSource pos Size8 (LocMem (R9, offset)) = Syntax.FromMem8 pos (fromIntegral offset) $ Syntax.R9 pos
_locToSource pos Size8 (LocReg R10) = Syntax.FromReg8 pos $ Syntax.R10B pos
_locToSource pos Size8 (LocMem (R10, offset)) = Syntax.FromMem8 pos (fromIntegral offset) $ Syntax.R10 pos
_locToSource pos Size8 (LocReg R11) = Syntax.FromReg8 pos $ Syntax.R11B pos
_locToSource pos Size8 (LocMem (R11, offset)) = Syntax.FromMem8 pos (fromIntegral offset) $ Syntax.R11 pos
_locToSource pos Size8 (LocReg R12) = Syntax.FromReg8 pos $ Syntax.R12B pos
_locToSource pos Size8 (LocMem (R12, offset)) = Syntax.FromMem8 pos (fromIntegral offset) $ Syntax.R12 pos
_locToSource pos Size8 (LocReg R13) = Syntax.FromReg8 pos $ Syntax.R13B pos
_locToSource pos Size8 (LocMem (R13, offset)) = Syntax.FromMem8 pos (fromIntegral offset) $ Syntax.R13 pos
_locToSource pos Size8 (LocReg R14) = Syntax.FromReg8 pos $ Syntax.R14B pos
_locToSource pos Size8 (LocMem (R14, offset)) = Syntax.FromMem8 pos (fromIntegral offset) $ Syntax.R14 pos
_locToSource pos Size8 (LocReg R15) = Syntax.FromReg8 pos $ Syntax.R15B pos
_locToSource pos Size8 (LocMem (R15, offset)) = Syntax.FromMem8 pos (fromIntegral offset) $ Syntax.R15 pos
_locToTarget :: a -> Size -> Loc -> Syntax.Target' a
_locToTarget pos Size64 (LocMemOffset ptrBase ptrIdx ptrOffset ptrScale) =
	let (Syntax.ToMem64 _ _ rBase) = _locToTarget pos Size64 (LocMem (ptrBase, 0)) in
	let (Syntax.ToMem64 _ _ rIdx) =  _locToTarget pos Size64 (LocMem (ptrIdx, 0)) in
	Syntax.ToMemComplex64 pos (fromIntegral ptrOffset) rBase rIdx (fromIntegral $ toBytes $ ptrScale)
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
_locToTarget pos Size32 (LocMemOffset ptrBase ptrIdx ptrOffset ptrScale) =
	let (Syntax.ToMem32 _ _ rBase) = _locToTarget pos Size32 (LocMem (ptrBase, 0)) in
	let (Syntax.ToMem32 _ _ rIdx) =  _locToTarget pos Size32 (LocMem (ptrIdx, 0)) in
	Syntax.ToMemComplex32 pos (fromIntegral ptrOffset) rBase rIdx (fromIntegral $ toBytes $ ptrScale)
_locToTarget pos Size32 (LocReg RAX) = Syntax.ToReg32 pos $ Syntax.EAX pos
_locToTarget pos Size32 (LocMem (RAX, offset)) = Syntax.ToMem32 pos (fromIntegral offset) $ Syntax.RAX pos
_locToTarget pos Size32 (LocReg RBX) = Syntax.ToReg32 pos $ Syntax.EBX pos
_locToTarget pos Size32 (LocMem (RBX, offset)) = Syntax.ToMem32 pos (fromIntegral offset) $ Syntax.RBX pos
_locToTarget pos Size32 (LocReg RCX) = Syntax.ToReg32 pos $ Syntax.ECX pos
_locToTarget pos Size32 (LocMem (RCX, offset)) = Syntax.ToMem32 pos (fromIntegral offset) $ Syntax.RCX pos
_locToTarget pos Size32 (LocReg RDX) = Syntax.ToReg32 pos $ Syntax.EDX pos
_locToTarget pos Size32 (LocMem (RDX, offset)) = Syntax.ToMem32 pos (fromIntegral offset) $ Syntax.RDX pos
_locToTarget pos Size32 (LocReg RDI) = Syntax.ToReg32 pos $ Syntax.EDI pos
_locToTarget pos Size32 (LocMem (RDI, offset)) = Syntax.ToMem32 pos (fromIntegral offset) $ Syntax.RDI pos
_locToTarget pos Size32 (LocReg RSI) = Syntax.ToReg32 pos $ Syntax.ESI pos
_locToTarget pos Size32 (LocMem (RSI, offset)) = Syntax.ToMem32 pos (fromIntegral offset) $ Syntax.RSI pos
_locToTarget pos Size32 (LocReg RSP) = Syntax.ToReg32 pos $ Syntax.ESP pos
_locToTarget pos Size32 (LocMem (RSP, offset)) = Syntax.ToMem32 pos (fromIntegral offset) $ Syntax.RSP pos
_locToTarget pos Size32 (LocReg RBP) = Syntax.ToReg32 pos $ Syntax.EBP pos
_locToTarget pos Size32 (LocMem (RBP, offset)) = Syntax.ToMem32 pos (fromIntegral offset) $ Syntax.RBP pos
_locToTarget pos Size32 (LocReg R8) = Syntax.ToReg32 pos $ Syntax.R8D pos
_locToTarget pos Size32 (LocMem (R8, offset)) = Syntax.ToMem32 pos (fromIntegral offset) $ Syntax.R8 pos
_locToTarget pos Size32 (LocReg R9) = Syntax.ToReg32 pos $ Syntax.R9D pos
_locToTarget pos Size32 (LocMem (R9, offset)) = Syntax.ToMem32 pos (fromIntegral offset) $ Syntax.R9 pos
_locToTarget pos Size32 (LocReg R10) = Syntax.ToReg32 pos $ Syntax.R10D pos
_locToTarget pos Size32 (LocMem (R10, offset)) = Syntax.ToMem32 pos (fromIntegral offset) $ Syntax.R10 pos
_locToTarget pos Size32 (LocReg R11) = Syntax.ToReg32 pos $ Syntax.R11D pos
_locToTarget pos Size32 (LocMem (R11, offset)) = Syntax.ToMem32 pos (fromIntegral offset) $ Syntax.R11 pos
_locToTarget pos Size32 (LocReg R12) = Syntax.ToReg32 pos $ Syntax.R12D pos
_locToTarget pos Size32 (LocMem (R12, offset)) = Syntax.ToMem32 pos (fromIntegral offset) $ Syntax.R12 pos
_locToTarget pos Size32 (LocReg R13) = Syntax.ToReg32 pos $ Syntax.R13D pos
_locToTarget pos Size32 (LocMem (R13, offset)) = Syntax.ToMem32 pos (fromIntegral offset) $ Syntax.R13 pos
_locToTarget pos Size32 (LocReg R14) = Syntax.ToReg32 pos $ Syntax.R14D pos
_locToTarget pos Size32 (LocMem (R14, offset)) = Syntax.ToMem32 pos (fromIntegral offset) $ Syntax.R14 pos
_locToTarget pos Size32 (LocReg R15) = Syntax.ToReg32 pos $ Syntax.R15D pos
_locToTarget pos Size32 (LocMem (R15, offset)) = Syntax.ToMem32 pos (fromIntegral offset) $ Syntax.R15 pos
_locToTarget pos Size16 (LocMemOffset ptrBase ptrIdx ptrOffset ptrScale) =
	let (Syntax.ToMem16 _ _ rBase) = _locToTarget pos Size16 (LocMem (ptrBase, 0)) in
	let (Syntax.ToMem16 _ _ rIdx) =  _locToTarget pos Size16 (LocMem (ptrIdx, 0)) in
	Syntax.ToMemComplex16 pos (fromIntegral ptrOffset) rBase rIdx (fromIntegral $ toBytes $ ptrScale)
_locToTarget pos Size16 (LocReg RAX) = Syntax.ToReg16 pos $ Syntax.AX pos
_locToTarget pos Size16 (LocMem (RAX, offset)) = Syntax.ToMem16 pos (fromIntegral offset) $ Syntax.RAX pos
_locToTarget pos Size16 (LocReg RBX) = Syntax.ToReg16 pos $ Syntax.BX pos
_locToTarget pos Size16 (LocMem (RBX, offset)) = Syntax.ToMem16 pos (fromIntegral offset) $ Syntax.RBX pos
_locToTarget pos Size16 (LocReg RCX) = Syntax.ToReg16 pos $ Syntax.CX pos
_locToTarget pos Size16 (LocMem (RCX, offset)) = Syntax.ToMem16 pos (fromIntegral offset) $ Syntax.RCX pos
_locToTarget pos Size16 (LocReg RDX) = Syntax.ToReg16 pos $ Syntax.DX pos
_locToTarget pos Size16 (LocMem (RDX, offset)) = Syntax.ToMem16 pos (fromIntegral offset) $ Syntax.RDX pos
_locToTarget pos Size16 (LocReg RDI) = Syntax.ToReg16 pos $ Syntax.DI pos
_locToTarget pos Size16 (LocMem (RDI, offset)) = Syntax.ToMem16 pos (fromIntegral offset) $ Syntax.RDI pos
_locToTarget pos Size16 (LocReg RSI) = Syntax.ToReg16 pos $ Syntax.SI pos
_locToTarget pos Size16 (LocMem (RSI, offset)) = Syntax.ToMem16 pos (fromIntegral offset) $ Syntax.RSI pos
_locToTarget pos Size16 (LocReg RSP) = Syntax.ToReg16 pos $ Syntax.SP pos
_locToTarget pos Size16 (LocMem (RSP, offset)) = Syntax.ToMem16 pos (fromIntegral offset) $ Syntax.RSP pos
_locToTarget pos Size16 (LocReg RBP) = Syntax.ToReg16 pos $ Syntax.BP pos
_locToTarget pos Size16 (LocMem (RBP, offset)) = Syntax.ToMem16 pos (fromIntegral offset) $ Syntax.RBP pos
_locToTarget pos Size16 (LocReg R8) = Syntax.ToReg16 pos $ Syntax.R8W pos
_locToTarget pos Size16 (LocMem (R8, offset)) = Syntax.ToMem16 pos (fromIntegral offset) $ Syntax.R8 pos
_locToTarget pos Size16 (LocReg R9) = Syntax.ToReg16 pos $ Syntax.R9W pos
_locToTarget pos Size16 (LocMem (R9, offset)) = Syntax.ToMem16 pos (fromIntegral offset) $ Syntax.R9 pos
_locToTarget pos Size16 (LocReg R10) = Syntax.ToReg16 pos $ Syntax.R10W pos
_locToTarget pos Size16 (LocMem (R10, offset)) = Syntax.ToMem16 pos (fromIntegral offset) $ Syntax.R10 pos
_locToTarget pos Size16 (LocReg R11) = Syntax.ToReg16 pos $ Syntax.R11W pos
_locToTarget pos Size16 (LocMem (R11, offset)) = Syntax.ToMem16 pos (fromIntegral offset) $ Syntax.R11 pos
_locToTarget pos Size16 (LocReg R12) = Syntax.ToReg16 pos $ Syntax.R12W pos
_locToTarget pos Size16 (LocMem (R12, offset)) = Syntax.ToMem16 pos (fromIntegral offset) $ Syntax.R12 pos
_locToTarget pos Size16 (LocReg R13) = Syntax.ToReg16 pos $ Syntax.R13W pos
_locToTarget pos Size16 (LocMem (R13, offset)) = Syntax.ToMem16 pos (fromIntegral offset) $ Syntax.R13 pos
_locToTarget pos Size16 (LocReg R14) = Syntax.ToReg16 pos $ Syntax.R14W pos
_locToTarget pos Size16 (LocMem (R14, offset)) = Syntax.ToMem16 pos (fromIntegral offset) $ Syntax.R14 pos
_locToTarget pos Size16 (LocReg R15) = Syntax.ToReg16 pos $ Syntax.R15W pos
_locToTarget pos Size16 (LocMem (R15, offset)) = Syntax.ToMem16 pos (fromIntegral offset) $ Syntax.R15 pos
_locToTarget pos Size8 (LocMemOffset ptrBase ptrIdx ptrOffset ptrScale) =
	let (Syntax.ToMem8 _ _ rBase) = _locToTarget pos Size8 (LocMem (ptrBase, 0)) in
	let (Syntax.ToMem8 _ _ rIdx) =  _locToTarget pos Size8 (LocMem (ptrIdx, 0)) in
	Syntax.ToMemComplex8 pos (fromIntegral ptrOffset) rBase rIdx (fromIntegral $ toBytes $ ptrScale)
_locToTarget pos Size8 (LocReg RAX) = Syntax.ToReg8 pos $ Syntax.AL pos
_locToTarget pos Size8 (LocMem (RAX, offset)) = Syntax.ToMem8 pos (fromIntegral offset) $ Syntax.RAX pos
_locToTarget pos Size8 (LocReg RBX) = Syntax.ToReg8 pos $ Syntax.BL pos
_locToTarget pos Size8 (LocMem (RBX, offset)) = Syntax.ToMem8 pos (fromIntegral offset) $ Syntax.RBX pos
_locToTarget pos Size8 (LocReg RCX) = Syntax.ToReg8 pos $ Syntax.CL pos
_locToTarget pos Size8 (LocMem (RCX, offset)) = Syntax.ToMem8 pos (fromIntegral offset) $ Syntax.RCX pos
_locToTarget pos Size8 (LocReg RDX) = Syntax.ToReg8 pos $ Syntax.DL pos
_locToTarget pos Size8 (LocMem (RDX, offset)) = Syntax.ToMem8 pos (fromIntegral offset) $ Syntax.RDX pos
_locToTarget pos Size8 (LocReg RDI) = Syntax.ToReg8 pos $ Syntax.DIL pos
_locToTarget pos Size8 (LocMem (RDI, offset)) = Syntax.ToMem8 pos (fromIntegral offset) $ Syntax.RDI pos
_locToTarget pos Size8 (LocReg RSI) = Syntax.ToReg8 pos $ Syntax.SIL pos
_locToTarget pos Size8 (LocMem (RSI, offset)) = Syntax.ToMem8 pos (fromIntegral offset) $ Syntax.RSI pos
_locToTarget pos Size8 (LocReg RSP) = Syntax.ToReg8 pos $ Syntax.SPL pos
_locToTarget pos Size8 (LocMem (RSP, offset)) = Syntax.ToMem8 pos (fromIntegral offset) $ Syntax.RSP pos
_locToTarget pos Size8 (LocReg RBP) = Syntax.ToReg8 pos $ Syntax.BPL pos
_locToTarget pos Size8 (LocMem (RBP, offset)) = Syntax.ToMem8 pos (fromIntegral offset) $ Syntax.RBP pos
_locToTarget pos Size8 (LocReg R8) = Syntax.ToReg8 pos $ Syntax.R8B pos
_locToTarget pos Size8 (LocMem (R8, offset)) = Syntax.ToMem8 pos (fromIntegral offset) $ Syntax.R8 pos
_locToTarget pos Size8 (LocReg R9) = Syntax.ToReg8 pos $ Syntax.R9B pos
_locToTarget pos Size8 (LocMem (R9, offset)) = Syntax.ToMem8 pos (fromIntegral offset) $ Syntax.R9 pos
_locToTarget pos Size8 (LocReg R10) = Syntax.ToReg8 pos $ Syntax.R10B pos
_locToTarget pos Size8 (LocMem (R10, offset)) = Syntax.ToMem8 pos (fromIntegral offset) $ Syntax.R10 pos
_locToTarget pos Size8 (LocReg R11) = Syntax.ToReg8 pos $ Syntax.R11B pos
_locToTarget pos Size8 (LocMem (R11, offset)) = Syntax.ToMem8 pos (fromIntegral offset) $ Syntax.R11 pos
_locToTarget pos Size8 (LocReg R12) = Syntax.ToReg8 pos $ Syntax.R12B pos
_locToTarget pos Size8 (LocMem (R12, offset)) = Syntax.ToMem8 pos (fromIntegral offset) $ Syntax.R12 pos
_locToTarget pos Size8 (LocReg R13) = Syntax.ToReg8 pos $ Syntax.R13B pos
_locToTarget pos Size8 (LocMem (R13, offset)) = Syntax.ToMem8 pos (fromIntegral offset) $ Syntax.R13 pos
_locToTarget pos Size8 (LocReg R14) = Syntax.ToReg8 pos $ Syntax.R14B pos
_locToTarget pos Size8 (LocMem (R14, offset)) = Syntax.ToMem8 pos (fromIntegral offset) $ Syntax.R14 pos
_locToTarget pos Size8 (LocReg R15) = Syntax.ToReg8 pos $ Syntax.R15B pos
_locToTarget pos Size8 (LocMem (R15, offset)) = Syntax.ToMem8 pos (fromIntegral offset) $ Syntax.R15 pos

-- Size classes

data Size = Size64 | Size32 | Size16 | Size8
	deriving (Eq, Ord, Show, Read, Generic, Typeable)

toBytes :: Size -> Int64

toBytes Size64 = 8
toBytes Size32 = 4
toBytes Size16 = 2
toBytes Size8 = 1

-- Instruction wrappers

data Instr a anno = CALL a String (Annotation a anno) | CALL_INDIRECT a Reg Integer (Annotation a anno) | Label a String (Annotation a anno) | ADD a Size (Loc) (Loc) (Annotation a anno) | AND a Size (Loc) (Loc) (Annotation a anno) | CMP a Size (Loc) (Loc) (Annotation a anno) | IMUL a Size (Loc) (Loc) (Annotation a anno) | LEA a Size (Loc) (Loc) (Annotation a anno) | MOV a Size (Loc) (Loc) (Annotation a anno) | SUB a Size (Loc) (Loc) (Annotation a anno) | TEST a Size (Loc) (Loc) (Annotation a anno) | XOR a Size (Loc) (Loc) (Annotation a anno) | XCHG a Size (Loc) (Loc) (Annotation a anno) | SAL a Size (Loc) (Loc) (Annotation a anno) | SAR a Size (Loc) (Loc) (Annotation a anno) | NEG a Size (Loc) (Annotation a anno) | IDIV a Size (Loc) (Annotation a anno) | INC a Size (Loc) (Annotation a anno) | DEC a Size (Loc) (Annotation a anno) | PUSH a Size (Loc) (Annotation a anno) | JMP a (String) (Annotation a anno) | J a (ValOrd) (String) (Annotation a anno) | SET a (ValOrd) (Loc) (Annotation a anno) | POP a (Loc) (Annotation a anno) | LEAVE a (Annotation a anno) | RET a (Annotation a anno) | CDQ a (Annotation a anno)
	deriving (Eq, Ord, Show, Read, Generic, Foldable, Traversable, Functor, Typeable)



label :: (Monad m) => a -> String -> (Maybe anno) -> ASMGeneratorT a anno m ()
label pos l ann =
	_emitInstr pos $ Label pos (sanitizeLabel l) $ maybe (NoAnnotation pos) (Annotation pos) ann


add :: (Monad m) => a -> Size -> Loc -> Loc -> (Maybe anno) -> ASMGeneratorT a anno m ()
add pos size loc1 loc2 ann =
	_emitInstr pos $ ADD pos size loc1 loc2 $ maybe (NoAnnotation pos) (Annotation pos) ann


and :: (Monad m) => a -> Size -> Loc -> Loc -> (Maybe anno) -> ASMGeneratorT a anno m ()
and pos size loc1 loc2 ann =
	_emitInstr pos $ AND pos size loc1 loc2 $ maybe (NoAnnotation pos) (Annotation pos) ann


cmp :: (Monad m) => a -> Size -> Loc -> Loc -> (Maybe anno) -> ASMGeneratorT a anno m ()
cmp pos size loc1 loc2 ann =
	_emitInstr pos $ CMP pos size loc1 loc2 $ maybe (NoAnnotation pos) (Annotation pos) ann


imul :: (Monad m) => a -> Size -> Loc -> Loc -> (Maybe anno) -> ASMGeneratorT a anno m ()
imul pos size loc1 loc2 ann =
	_emitInstr pos $ IMUL pos size loc1 loc2 $ maybe (NoAnnotation pos) (Annotation pos) ann


lea :: (Monad m) => a -> Size -> Loc -> Loc -> (Maybe anno) -> ASMGeneratorT a anno m ()
lea pos size loc1 loc2 ann =
	_emitInstr pos $ LEA pos size loc1 loc2 $ maybe (NoAnnotation pos) (Annotation pos) ann


mov :: (Monad m) => a -> Size -> Loc -> Loc -> (Maybe anno) -> ASMGeneratorT a anno m ()
mov pos size loc1 loc2 ann =
	_emitInstr pos $ MOV pos size loc1 loc2 $ maybe (NoAnnotation pos) (Annotation pos) ann


sub :: (Monad m) => a -> Size -> Loc -> Loc -> (Maybe anno) -> ASMGeneratorT a anno m ()
sub pos size loc1 loc2 ann =
	_emitInstr pos $ SUB pos size loc1 loc2 $ maybe (NoAnnotation pos) (Annotation pos) ann


test :: (Monad m) => a -> Size -> Loc -> Loc -> (Maybe anno) -> ASMGeneratorT a anno m ()
test pos size loc1 loc2 ann =
	_emitInstr pos $ TEST pos size loc1 loc2 $ maybe (NoAnnotation pos) (Annotation pos) ann


xor :: (Monad m) => a -> Size -> Loc -> Loc -> (Maybe anno) -> ASMGeneratorT a anno m ()
xor pos size loc1 loc2 ann =
	_emitInstr pos $ XOR pos size loc1 loc2 $ maybe (NoAnnotation pos) (Annotation pos) ann


xchg :: (Monad m) => a -> Size -> Loc -> Loc -> (Maybe anno) -> ASMGeneratorT a anno m ()
xchg pos size loc1 loc2 ann =
	_emitInstr pos $ XCHG pos size loc1 loc2 $ maybe (NoAnnotation pos) (Annotation pos) ann


sal :: (Monad m) => a -> Size -> Loc -> Loc -> (Maybe anno) -> ASMGeneratorT a anno m ()
sal pos size loc1 loc2 ann =
	_emitInstr pos $ SAL pos size loc1 loc2 $ maybe (NoAnnotation pos) (Annotation pos) ann


sar :: (Monad m) => a -> Size -> Loc -> Loc -> (Maybe anno) -> ASMGeneratorT a anno m ()
sar pos size loc1 loc2 ann =
	_emitInstr pos $ SAR pos size loc1 loc2 $ maybe (NoAnnotation pos) (Annotation pos) ann


neg :: (Monad m) => a -> Size -> Loc -> (Maybe anno) -> ASMGeneratorT a anno m ()
neg pos size loc ann =
	_emitInstr pos $ NEG pos size loc $ maybe (NoAnnotation pos) (Annotation pos) ann


idiv :: (Monad m) => a -> Size -> Loc -> (Maybe anno) -> ASMGeneratorT a anno m ()
idiv pos size loc ann =
	_emitInstr pos $ IDIV pos size loc $ maybe (NoAnnotation pos) (Annotation pos) ann


inc :: (Monad m) => a -> Size -> Loc -> (Maybe anno) -> ASMGeneratorT a anno m ()
inc pos size loc ann =
	_emitInstr pos $ INC pos size loc $ maybe (NoAnnotation pos) (Annotation pos) ann


dec :: (Monad m) => a -> Size -> Loc -> (Maybe anno) -> ASMGeneratorT a anno m ()
dec pos size loc ann =
	_emitInstr pos $ DEC pos size loc $ maybe (NoAnnotation pos) (Annotation pos) ann


push :: (Monad m) => a -> Size -> Loc -> (Maybe anno) -> ASMGeneratorT a anno m ()
push pos size loc ann =
	_emitInstr pos $ PUSH pos size loc $ maybe (NoAnnotation pos) (Annotation pos) ann


sete :: (Monad m) => a -> Loc -> (Maybe anno) -> ASMGeneratorT a anno m ()
sete pos loc ann =
	_emitInstr pos $ SET pos OrdE loc $ maybe (NoAnnotation pos) (Annotation pos) ann


setg :: (Monad m) => a -> Loc -> (Maybe anno) -> ASMGeneratorT a anno m ()
setg pos loc ann =
	_emitInstr pos $ SET pos OrdG loc $ maybe (NoAnnotation pos) (Annotation pos) ann


setge :: (Monad m) => a -> Loc -> (Maybe anno) -> ASMGeneratorT a anno m ()
setge pos loc ann =
	_emitInstr pos $ SET pos OrdGE loc $ maybe (NoAnnotation pos) (Annotation pos) ann


setl :: (Monad m) => a -> Loc -> (Maybe anno) -> ASMGeneratorT a anno m ()
setl pos loc ann =
	_emitInstr pos $ SET pos OrdL loc $ maybe (NoAnnotation pos) (Annotation pos) ann


setle :: (Monad m) => a -> Loc -> (Maybe anno) -> ASMGeneratorT a anno m ()
setle pos loc ann =
	_emitInstr pos $ SET pos OrdLE loc $ maybe (NoAnnotation pos) (Annotation pos) ann


setne :: (Monad m) => a -> Loc -> (Maybe anno) -> ASMGeneratorT a anno m ()
setne pos loc ann =
	_emitInstr pos $ SET pos OrdNE loc $ maybe (NoAnnotation pos) (Annotation pos) ann


setz :: (Monad m) => a -> Loc -> (Maybe anno) -> ASMGeneratorT a anno m ()
setz pos loc ann =
	_emitInstr pos $ SET pos OrdZ loc $ maybe (NoAnnotation pos) (Annotation pos) ann


setnz :: (Monad m) => a -> Loc -> (Maybe anno) -> ASMGeneratorT a anno m ()
setnz pos loc ann =
	_emitInstr pos $ SET pos OrdNZ loc $ maybe (NoAnnotation pos) (Annotation pos) ann


pop :: (Monad m) => a -> Loc -> (Maybe anno) -> ASMGeneratorT a anno m ()
pop pos loc ann =
	_emitInstr pos $ POP pos loc $ maybe (NoAnnotation pos) (Annotation pos) ann


leave :: (Monad m) => a -> (Maybe anno) -> ASMGeneratorT a anno m ()
leave pos ann =
	_emitInstr pos $ LEAVE pos $ maybe (NoAnnotation pos) (Annotation pos) ann


ret :: (Monad m) => a -> (Maybe anno) -> ASMGeneratorT a anno m ()
ret pos ann =
	_emitInstr pos $ RET pos $ maybe (NoAnnotation pos) (Annotation pos) ann


cdq :: (Monad m) => a -> (Maybe anno) -> ASMGeneratorT a anno m ()
cdq pos ann =
	_emitInstr pos $ CDQ pos $ maybe (NoAnnotation pos) (Annotation pos) ann


jmp :: (Monad m) => a -> String -> (Maybe anno) -> ASMGeneratorT a anno m ()
jmp pos label ann =
	_emitInstr pos $ JMP pos label $ maybe (NoAnnotation pos) (Annotation pos) ann


je :: (Monad m) => a -> String -> (Maybe anno) -> ASMGeneratorT a anno m ()
je pos label ann =
	_emitInstr pos $ J pos OrdE label $ maybe (NoAnnotation pos) (Annotation pos) ann


jg :: (Monad m) => a -> String -> (Maybe anno) -> ASMGeneratorT a anno m ()
jg pos label ann =
	_emitInstr pos $ J pos OrdG label $ maybe (NoAnnotation pos) (Annotation pos) ann


jge :: (Monad m) => a -> String -> (Maybe anno) -> ASMGeneratorT a anno m ()
jge pos label ann =
	_emitInstr pos $ J pos OrdGE label $ maybe (NoAnnotation pos) (Annotation pos) ann


jl :: (Monad m) => a -> String -> (Maybe anno) -> ASMGeneratorT a anno m ()
jl pos label ann =
	_emitInstr pos $ J pos OrdL label $ maybe (NoAnnotation pos) (Annotation pos) ann


jle :: (Monad m) => a -> String -> (Maybe anno) -> ASMGeneratorT a anno m ()
jle pos label ann =
	_emitInstr pos $ J pos OrdLE label $ maybe (NoAnnotation pos) (Annotation pos) ann


jne :: (Monad m) => a -> String -> (Maybe anno) -> ASMGeneratorT a anno m ()
jne pos label ann =
	_emitInstr pos $ J pos OrdNE label $ maybe (NoAnnotation pos) (Annotation pos) ann


jz :: (Monad m) => a -> String -> (Maybe anno) -> ASMGeneratorT a anno m ()
jz pos label ann =
	_emitInstr pos $ J pos OrdZ label $ maybe (NoAnnotation pos) (Annotation pos) ann


jnz :: (Monad m) => a -> String -> (Maybe anno) -> ASMGeneratorT a anno m ()
jnz pos label ann =
	_emitInstr pos $ J pos OrdNZ label $ maybe (NoAnnotation pos) (Annotation pos) ann


call :: (Monad m) => a -> String -> (Maybe anno) -> ASMGeneratorT a anno m ()
call pos label ann =
	_emitInstr pos $ CALL pos label $ maybe (NoAnnotation pos) (Annotation pos) ann
callIndirect :: (Monad m) => a -> Reg -> Integer -> (Maybe anno) -> ASMGeneratorT a anno m ()
callIndirect pos reg offset ann =
	_emitInstr pos $ CALL_INDIRECT pos reg offset $ maybe (NoAnnotation pos) (Annotation pos) ann


instance LocFunctor (Instr a anno1) where
	mapLoc fn (CALL_INDIRECT pos reg offset ann) = let (LocReg reg') = fn (LocReg reg) in CALL_INDIRECT pos reg' offset ann

	mapLoc fn (ADD pos size from to ann) = ADD pos size (fn from) (fn to) ann
	mapLoc fn (AND pos size from to ann) = AND pos size (fn from) (fn to) ann
	mapLoc fn (CMP pos size from to ann) = CMP pos size (fn from) (fn to) ann
	mapLoc fn (IMUL pos size from to ann) = IMUL pos size (fn from) (fn to) ann
	mapLoc fn (LEA pos size from to ann) = LEA pos size (fn from) (fn to) ann
	mapLoc fn (MOV pos size from to ann) = MOV pos size (fn from) (fn to) ann
	mapLoc fn (SUB pos size from to ann) = SUB pos size (fn from) (fn to) ann
	mapLoc fn (TEST pos size from to ann) = TEST pos size (fn from) (fn to) ann
	mapLoc fn (XOR pos size from to ann) = XOR pos size (fn from) (fn to) ann
	mapLoc fn (XCHG pos size from to ann) = XCHG pos size (fn from) (fn to) ann
	mapLoc fn (SAL pos size from to ann) = SAL pos size (fn from) (fn to) ann
	mapLoc fn (SAR pos size from to ann) = SAR pos size (fn from) (fn to) ann
	mapLoc fn (NEG pos size to ann) = NEG pos size (fn to) ann
	mapLoc fn (IDIV pos size to ann) = IDIV pos size (fn to) ann
	mapLoc fn (INC pos size to ann) = INC pos size (fn to) ann
	mapLoc fn (DEC pos size to ann) = DEC pos size (fn to) ann
	mapLoc fn (PUSH pos size from ann) = PUSH pos size (fn from) ann
	mapLoc fn (SET pos ordVal loc ann) = SET pos ordVal (fn loc) ann
	mapLoc fn (POP pos loc ann) = POP pos (fn loc) ann
	mapLoc fn instr = instr
mapInstrData :: (a -> b) -> (anno1 -> anno2) -> Instr a anno1 -> Instr b anno2
mapInstrData f g (CALL_INDIRECT pos reg offset ann) = CALL_INDIRECT (f pos) reg offset (mapAnnotationData f g ann)
mapInstrData f g (CALL pos label ann) = CALL (f pos) label (mapAnnotationData f g ann)
mapInstrData f g (Label pos label ann) = Label (f pos) label (mapAnnotationData f g ann)
mapInstrData f g (ADD pos size from to ann) = ADD (f pos) size from to (mapAnnotationData f g ann)
mapInstrData f g (AND pos size from to ann) = AND (f pos) size from to (mapAnnotationData f g ann)
mapInstrData f g (CMP pos size from to ann) = CMP (f pos) size from to (mapAnnotationData f g ann)
mapInstrData f g (IMUL pos size from to ann) = IMUL (f pos) size from to (mapAnnotationData f g ann)
mapInstrData f g (LEA pos size from to ann) = LEA (f pos) size from to (mapAnnotationData f g ann)
mapInstrData f g (MOV pos size from to ann) = MOV (f pos) size from to (mapAnnotationData f g ann)
mapInstrData f g (SUB pos size from to ann) = SUB (f pos) size from to (mapAnnotationData f g ann)
mapInstrData f g (TEST pos size from to ann) = TEST (f pos) size from to (mapAnnotationData f g ann)
mapInstrData f g (XOR pos size from to ann) = XOR (f pos) size from to (mapAnnotationData f g ann)
mapInstrData f g (XCHG pos size from to ann) = XCHG (f pos) size from to (mapAnnotationData f g ann)
mapInstrData f g (SAL pos size from to ann) = SAL (f pos) size from to (mapAnnotationData f g ann)
mapInstrData f g (SAR pos size from to ann) = SAR (f pos) size from to (mapAnnotationData f g ann)
mapInstrData f g (NEG pos size to ann) = NEG (f pos) size to (mapAnnotationData f g ann)
mapInstrData f g (IDIV pos size to ann) = IDIV (f pos) size to (mapAnnotationData f g ann)
mapInstrData f g (INC pos size to ann) = INC (f pos) size to (mapAnnotationData f g ann)
mapInstrData f g (DEC pos size to ann) = DEC (f pos) size to (mapAnnotationData f g ann)
mapInstrData f g (PUSH pos size from ann) = PUSH (f pos) size from (mapAnnotationData f g ann)
mapInstrData f g (SET pos ordVal loc ann) = SET (f pos) ordVal loc (mapAnnotationData f g ann)
mapInstrData f g (POP pos loc ann) = POP (f pos) loc (mapAnnotationData f g ann)
mapInstrData f g (JMP pos label ann) = JMP (f pos) label (mapAnnotationData f g ann)
mapInstrData f g (J pos ordVal label ann) = J (f pos) ordVal label (mapAnnotationData f g ann)
mapInstrData f g (LEAVE pos ann) = LEAVE (f pos) (mapAnnotationData f g ann)
mapInstrData f g (RET pos ann) = RET (f pos) (mapAnnotationData f g ann)
mapInstrData f g (CDQ pos ann) = CDQ (f pos) (mapAnnotationData f g ann)
getInstrUsedLabels :: (Instr a anno) -> [String]
getInstrUsedLabels (CALL _ label _) = [label]
getInstrUsedLabels (Label _ label _) = [label]
getInstrUsedLabels (J _ _ label _) = [label]
getInstrUsedLabels (JMP _ label _) = [label]
getInstrUsedLabels _ = []
_convertInstr :: (Monad m, CommentProvider a anno) => Instr a anno -> ASMGeneratorT a anno m (Syntax.AsmInstr' a)
_convertInstr (Label pos l ann) = return $ Syntax.LabelDef pos (Syntax.Label $ sanitizeLabel l) (_convert_annotation ann)
_convertInstr (ADD pos Size64 loc1 loc2 ann) = return $ Syntax.ADD64 pos (_locToSource pos Size64 loc1) (_locToTarget pos Size64 loc2) (_convert_annotation ann)
_convertInstr (ADD pos Size32 loc1 loc2 ann) = return $ Syntax.ADD32 pos (_locToSource pos Size32 loc1) (_locToTarget pos Size32 loc2) (_convert_annotation ann)
_convertInstr (ADD pos Size16 loc1 loc2 ann) = return $ Syntax.ADD16 pos (_locToSource pos Size16 loc1) (_locToTarget pos Size16 loc2) (_convert_annotation ann)
_convertInstr (ADD pos Size8 loc1 loc2 ann) = return $ Syntax.ADD8 pos (_locToSource pos Size8 loc1) (_locToTarget pos Size8 loc2) (_convert_annotation ann)
_convertInstr (ADD pos size _ _ _) = generatorFail $ EDataUnexpectedSize pos size
_convertInstr (AND pos Size64 loc1 loc2 ann) = return $ Syntax.AND64 pos (_locToSource pos Size64 loc1) (_locToTarget pos Size64 loc2) (_convert_annotation ann)
_convertInstr (AND pos Size32 loc1 loc2 ann) = return $ Syntax.AND32 pos (_locToSource pos Size32 loc1) (_locToTarget pos Size32 loc2) (_convert_annotation ann)
_convertInstr (AND pos Size16 loc1 loc2 ann) = return $ Syntax.AND16 pos (_locToSource pos Size16 loc1) (_locToTarget pos Size16 loc2) (_convert_annotation ann)
_convertInstr (AND pos Size8 loc1 loc2 ann) = return $ Syntax.AND8 pos (_locToSource pos Size8 loc1) (_locToTarget pos Size8 loc2) (_convert_annotation ann)
_convertInstr (AND pos size _ _ _) = generatorFail $ EDataUnexpectedSize pos size
_convertInstr (CMP pos Size64 loc1 loc2 ann) = return $ Syntax.CMP64 pos (_locToSource pos Size64 loc1) (_locToTarget pos Size64 loc2) (_convert_annotation ann)
_convertInstr (CMP pos Size32 loc1 loc2 ann) = return $ Syntax.CMP32 pos (_locToSource pos Size32 loc1) (_locToTarget pos Size32 loc2) (_convert_annotation ann)
_convertInstr (CMP pos Size16 loc1 loc2 ann) = return $ Syntax.CMP16 pos (_locToSource pos Size16 loc1) (_locToTarget pos Size16 loc2) (_convert_annotation ann)
_convertInstr (CMP pos Size8 loc1 loc2 ann) = return $ Syntax.CMP8 pos (_locToSource pos Size8 loc1) (_locToTarget pos Size8 loc2) (_convert_annotation ann)
_convertInstr (CMP pos size _ _ _) = generatorFail $ EDataUnexpectedSize pos size
_convertInstr (IMUL pos Size64 loc1 loc2 ann) = return $ Syntax.IMUL64 pos (_locToSource pos Size64 loc1) (_locToTarget pos Size64 loc2) (_convert_annotation ann)
_convertInstr (IMUL pos Size32 loc1 loc2 ann) = return $ Syntax.IMUL32 pos (_locToSource pos Size32 loc1) (_locToTarget pos Size32 loc2) (_convert_annotation ann)
_convertInstr (IMUL pos Size16 loc1 loc2 ann) = return $ Syntax.IMUL16 pos (_locToSource pos Size16 loc1) (_locToTarget pos Size16 loc2) (_convert_annotation ann)
_convertInstr (IMUL pos Size8 loc1 loc2 ann) = return $ Syntax.IMUL8 pos (_locToSource pos Size8 loc1) (_locToTarget pos Size8 loc2) (_convert_annotation ann)
_convertInstr (IMUL pos size _ _ _) = generatorFail $ EDataUnexpectedSize pos size
_convertInstr (LEA pos Size64 loc1 loc2 ann) = return $ Syntax.LEA64 pos (_locToSource pos Size64 loc1) (_locToTarget pos Size64 loc2) (_convert_annotation ann)
_convertInstr (LEA pos Size32 loc1 loc2 ann) = return $ Syntax.LEA32 pos (_locToSource pos Size32 loc1) (_locToTarget pos Size32 loc2) (_convert_annotation ann)
_convertInstr (LEA pos Size16 loc1 loc2 ann) = return $ Syntax.LEA16 pos (_locToSource pos Size16 loc1) (_locToTarget pos Size16 loc2) (_convert_annotation ann)
_convertInstr (LEA pos Size8 loc1 loc2 ann) = return $ Syntax.LEA8 pos (_locToSource pos Size8 loc1) (_locToTarget pos Size8 loc2) (_convert_annotation ann)
_convertInstr (LEA pos size _ _ _) = generatorFail $ EDataUnexpectedSize pos size
_convertInstr (MOV pos Size64 loc1 loc2 ann) = return $ Syntax.MOV64 pos (_locToSource pos Size64 loc1) (_locToTarget pos Size64 loc2) (_convert_annotation ann)
_convertInstr (MOV pos Size32 loc1 loc2 ann) = return $ Syntax.MOV32 pos (_locToSource pos Size32 loc1) (_locToTarget pos Size32 loc2) (_convert_annotation ann)
_convertInstr (MOV pos Size16 loc1 loc2 ann) = return $ Syntax.MOV16 pos (_locToSource pos Size16 loc1) (_locToTarget pos Size16 loc2) (_convert_annotation ann)
_convertInstr (MOV pos Size8 loc1 loc2 ann) = return $ Syntax.MOV8 pos (_locToSource pos Size8 loc1) (_locToTarget pos Size8 loc2) (_convert_annotation ann)
_convertInstr (MOV pos size _ _ _) = generatorFail $ EDataUnexpectedSize pos size
_convertInstr (SUB pos Size64 loc1 loc2 ann) = return $ Syntax.SUB64 pos (_locToSource pos Size64 loc1) (_locToTarget pos Size64 loc2) (_convert_annotation ann)
_convertInstr (SUB pos Size32 loc1 loc2 ann) = return $ Syntax.SUB32 pos (_locToSource pos Size32 loc1) (_locToTarget pos Size32 loc2) (_convert_annotation ann)
_convertInstr (SUB pos Size16 loc1 loc2 ann) = return $ Syntax.SUB16 pos (_locToSource pos Size16 loc1) (_locToTarget pos Size16 loc2) (_convert_annotation ann)
_convertInstr (SUB pos Size8 loc1 loc2 ann) = return $ Syntax.SUB8 pos (_locToSource pos Size8 loc1) (_locToTarget pos Size8 loc2) (_convert_annotation ann)
_convertInstr (SUB pos size _ _ _) = generatorFail $ EDataUnexpectedSize pos size
_convertInstr (TEST pos Size64 loc1 loc2 ann) = return $ Syntax.TEST64 pos (_locToSource pos Size64 loc1) (_locToTarget pos Size64 loc2) (_convert_annotation ann)
_convertInstr (TEST pos Size32 loc1 loc2 ann) = return $ Syntax.TEST32 pos (_locToSource pos Size32 loc1) (_locToTarget pos Size32 loc2) (_convert_annotation ann)
_convertInstr (TEST pos Size16 loc1 loc2 ann) = return $ Syntax.TEST16 pos (_locToSource pos Size16 loc1) (_locToTarget pos Size16 loc2) (_convert_annotation ann)
_convertInstr (TEST pos Size8 loc1 loc2 ann) = return $ Syntax.TEST8 pos (_locToSource pos Size8 loc1) (_locToTarget pos Size8 loc2) (_convert_annotation ann)
_convertInstr (TEST pos size _ _ _) = generatorFail $ EDataUnexpectedSize pos size
_convertInstr (XOR pos Size64 loc1 loc2 ann) = return $ Syntax.XOR64 pos (_locToSource pos Size64 loc1) (_locToTarget pos Size64 loc2) (_convert_annotation ann)
_convertInstr (XOR pos Size32 loc1 loc2 ann) = return $ Syntax.XOR32 pos (_locToSource pos Size32 loc1) (_locToTarget pos Size32 loc2) (_convert_annotation ann)
_convertInstr (XOR pos Size16 loc1 loc2 ann) = return $ Syntax.XOR16 pos (_locToSource pos Size16 loc1) (_locToTarget pos Size16 loc2) (_convert_annotation ann)
_convertInstr (XOR pos Size8 loc1 loc2 ann) = return $ Syntax.XOR8 pos (_locToSource pos Size8 loc1) (_locToTarget pos Size8 loc2) (_convert_annotation ann)
_convertInstr (XOR pos size _ _ _) = generatorFail $ EDataUnexpectedSize pos size
_convertInstr (XCHG pos Size64 loc1 loc2 ann) = return $ Syntax.XCHG64 pos (_locToSource pos Size64 loc1) (_locToTarget pos Size64 loc2) (_convert_annotation ann)
_convertInstr (XCHG pos Size32 loc1 loc2 ann) = return $ Syntax.XCHG32 pos (_locToSource pos Size32 loc1) (_locToTarget pos Size32 loc2) (_convert_annotation ann)
_convertInstr (XCHG pos Size16 loc1 loc2 ann) = return $ Syntax.XCHG16 pos (_locToSource pos Size16 loc1) (_locToTarget pos Size16 loc2) (_convert_annotation ann)
_convertInstr (XCHG pos Size8 loc1 loc2 ann) = return $ Syntax.XCHG8 pos (_locToSource pos Size8 loc1) (_locToTarget pos Size8 loc2) (_convert_annotation ann)
_convertInstr (XCHG pos size _ _ _) = generatorFail $ EDataUnexpectedSize pos size
_convertInstr (SAL pos Size64 loc1 loc2 ann) = return $ Syntax.SAL64 pos (_locToSource pos Size64 loc1) (_locToTarget pos Size64 loc2) (_convert_annotation ann)
_convertInstr (SAL pos Size32 loc1 loc2 ann) = return $ Syntax.SAL32 pos (_locToSource pos Size32 loc1) (_locToTarget pos Size32 loc2) (_convert_annotation ann)
_convertInstr (SAL pos Size16 loc1 loc2 ann) = return $ Syntax.SAL16 pos (_locToSource pos Size16 loc1) (_locToTarget pos Size16 loc2) (_convert_annotation ann)
_convertInstr (SAL pos Size8 loc1 loc2 ann) = return $ Syntax.SAL8 pos (_locToSource pos Size8 loc1) (_locToTarget pos Size8 loc2) (_convert_annotation ann)
_convertInstr (SAL pos size _ _ _) = generatorFail $ EDataUnexpectedSize pos size
_convertInstr (SAR pos Size64 loc1 loc2 ann) = return $ Syntax.SAR64 pos (_locToSource pos Size64 loc1) (_locToTarget pos Size64 loc2) (_convert_annotation ann)
_convertInstr (SAR pos Size32 loc1 loc2 ann) = return $ Syntax.SAR32 pos (_locToSource pos Size32 loc1) (_locToTarget pos Size32 loc2) (_convert_annotation ann)
_convertInstr (SAR pos Size16 loc1 loc2 ann) = return $ Syntax.SAR16 pos (_locToSource pos Size16 loc1) (_locToTarget pos Size16 loc2) (_convert_annotation ann)
_convertInstr (SAR pos Size8 loc1 loc2 ann) = return $ Syntax.SAR8 pos (_locToSource pos Size8 loc1) (_locToTarget pos Size8 loc2) (_convert_annotation ann)
_convertInstr (SAR pos size _ _ _) = generatorFail $ EDataUnexpectedSize pos size
_convertInstr (NEG pos Size64 loc ann) = return $ Syntax.NEG64 pos (_locToTarget pos Size64 loc) (_convert_annotation ann)
_convertInstr (NEG pos Size32 loc ann) = return $ Syntax.NEG32 pos (_locToTarget pos Size32 loc) (_convert_annotation ann)
_convertInstr (NEG pos Size16 loc ann) = return $ Syntax.NEG16 pos (_locToTarget pos Size16 loc) (_convert_annotation ann)
_convertInstr (NEG pos Size8 loc ann) = return $ Syntax.NEG8 pos (_locToTarget pos Size8 loc) (_convert_annotation ann)
_convertInstr (NEG pos size _ _) = generatorFail $ EDataUnexpectedSize pos size
_convertInstr (IDIV pos Size64 loc ann) = return $ Syntax.IDIV64 pos (_locToTarget pos Size64 loc) (_convert_annotation ann)
_convertInstr (IDIV pos Size32 loc ann) = return $ Syntax.IDIV32 pos (_locToTarget pos Size32 loc) (_convert_annotation ann)
_convertInstr (IDIV pos Size16 loc ann) = return $ Syntax.IDIV16 pos (_locToTarget pos Size16 loc) (_convert_annotation ann)
_convertInstr (IDIV pos Size8 loc ann) = return $ Syntax.IDIV8 pos (_locToTarget pos Size8 loc) (_convert_annotation ann)
_convertInstr (IDIV pos size _ _) = generatorFail $ EDataUnexpectedSize pos size
_convertInstr (INC pos Size64 loc ann) = return $ Syntax.INC64 pos (_locToTarget pos Size64 loc) (_convert_annotation ann)
_convertInstr (INC pos Size32 loc ann) = return $ Syntax.INC32 pos (_locToTarget pos Size32 loc) (_convert_annotation ann)
_convertInstr (INC pos Size16 loc ann) = return $ Syntax.INC16 pos (_locToTarget pos Size16 loc) (_convert_annotation ann)
_convertInstr (INC pos Size8 loc ann) = return $ Syntax.INC8 pos (_locToTarget pos Size8 loc) (_convert_annotation ann)
_convertInstr (INC pos size _ _) = generatorFail $ EDataUnexpectedSize pos size
_convertInstr (DEC pos Size64 loc ann) = return $ Syntax.DEC64 pos (_locToTarget pos Size64 loc) (_convert_annotation ann)
_convertInstr (DEC pos Size32 loc ann) = return $ Syntax.DEC32 pos (_locToTarget pos Size32 loc) (_convert_annotation ann)
_convertInstr (DEC pos Size16 loc ann) = return $ Syntax.DEC16 pos (_locToTarget pos Size16 loc) (_convert_annotation ann)
_convertInstr (DEC pos Size8 loc ann) = return $ Syntax.DEC8 pos (_locToTarget pos Size8 loc) (_convert_annotation ann)
_convertInstr (DEC pos size _ _) = generatorFail $ EDataUnexpectedSize pos size
_convertInstr (PUSH pos Size64 loc ann) = return $ Syntax.PUSH64 pos (_locToSource pos Size64 loc) (_convert_annotation ann)
_convertInstr (PUSH pos Size32 loc ann) = return $ Syntax.PUSH32 pos (_locToSource pos Size32 loc) (_convert_annotation ann)
_convertInstr (PUSH pos Size16 loc ann) = return $ Syntax.PUSH16 pos (_locToSource pos Size16 loc) (_convert_annotation ann)
_convertInstr (PUSH pos Size8 loc ann) = return $ Syntax.PUSH8 pos (_locToSource pos Size8 loc) (_convert_annotation ann)
_convertInstr (PUSH pos size _ _) = generatorFail $ EDataUnexpectedSize pos size
_convertInstr (SET pos OrdE loc ann) = let (Syntax.ToReg8 _ r) = (_locToTarget pos Size8 loc) in return $ Syntax.SETE pos r (_convert_annotation ann)
_convertInstr (SET pos OrdE loc _) = generatorFail $ ENonRegisterLocationGiven pos "SET" loc
_convertInstr (SET pos OrdG loc ann) = let (Syntax.ToReg8 _ r) = (_locToTarget pos Size8 loc) in return $ Syntax.SETG pos r (_convert_annotation ann)
_convertInstr (SET pos OrdG loc _) = generatorFail $ ENonRegisterLocationGiven pos "SET" loc
_convertInstr (SET pos OrdGE loc ann) = let (Syntax.ToReg8 _ r) = (_locToTarget pos Size8 loc) in return $ Syntax.SETGE pos r (_convert_annotation ann)
_convertInstr (SET pos OrdGE loc _) = generatorFail $ ENonRegisterLocationGiven pos "SET" loc
_convertInstr (SET pos OrdL loc ann) = let (Syntax.ToReg8 _ r) = (_locToTarget pos Size8 loc) in return $ Syntax.SETL pos r (_convert_annotation ann)
_convertInstr (SET pos OrdL loc _) = generatorFail $ ENonRegisterLocationGiven pos "SET" loc
_convertInstr (SET pos OrdLE loc ann) = let (Syntax.ToReg8 _ r) = (_locToTarget pos Size8 loc) in return $ Syntax.SETLE pos r (_convert_annotation ann)
_convertInstr (SET pos OrdLE loc _) = generatorFail $ ENonRegisterLocationGiven pos "SET" loc
_convertInstr (SET pos OrdNE loc ann) = let (Syntax.ToReg8 _ r) = (_locToTarget pos Size8 loc) in return $ Syntax.SETNE pos r (_convert_annotation ann)
_convertInstr (SET pos OrdNE loc _) = generatorFail $ ENonRegisterLocationGiven pos "SET" loc
_convertInstr (SET pos OrdZ loc ann) = let (Syntax.ToReg8 _ r) = (_locToTarget pos Size8 loc) in return $ Syntax.SETZ pos r (_convert_annotation ann)
_convertInstr (SET pos OrdZ loc _) = generatorFail $ ENonRegisterLocationGiven pos "SET" loc
_convertInstr (SET pos OrdNZ loc ann) = let (Syntax.ToReg8 _ r) = (_locToTarget pos Size8 loc) in return $ Syntax.SETNZ pos r (_convert_annotation ann)
_convertInstr (SET pos OrdNZ loc _) = generatorFail $ ENonRegisterLocationGiven pos "SET" loc
_convertInstr (POP pos loc@(LocReg reg) ann) = let (Syntax.ToReg64 _ r) = (_locToTarget pos Size64 loc) in return $ Syntax.POP pos r (_convert_annotation ann)
_convertInstr (POP pos loc _) = generatorFail $ ENonRegisterLocationGiven pos "POP" loc
_convertInstr (JMP pos label ann) = return $ Syntax.JMP pos (Syntax.Label $ sanitizeLabel label) (_convert_annotation ann)
_convertInstr (J pos OrdE label ann) = return $ Syntax.JE pos (Syntax.Label $ sanitizeLabel label) (_convert_annotation ann)
_convertInstr (J pos OrdG label ann) = return $ Syntax.JG pos (Syntax.Label $ sanitizeLabel label) (_convert_annotation ann)
_convertInstr (J pos OrdGE label ann) = return $ Syntax.JGE pos (Syntax.Label $ sanitizeLabel label) (_convert_annotation ann)
_convertInstr (J pos OrdL label ann) = return $ Syntax.JL pos (Syntax.Label $ sanitizeLabel label) (_convert_annotation ann)
_convertInstr (J pos OrdLE label ann) = return $ Syntax.JLE pos (Syntax.Label $ sanitizeLabel label) (_convert_annotation ann)
_convertInstr (J pos OrdNE label ann) = return $ Syntax.JNE pos (Syntax.Label $ sanitizeLabel label) (_convert_annotation ann)
_convertInstr (J pos OrdZ label ann) = return $ Syntax.JZ pos (Syntax.Label $ sanitizeLabel label) (_convert_annotation ann)
_convertInstr (J pos OrdNZ label ann) = return $ Syntax.JNZ pos (Syntax.Label $ sanitizeLabel label) (_convert_annotation ann)
_convertInstr (LEAVE pos ann) = return $ Syntax.LEAVE pos (_convert_annotation ann)
_convertInstr (RET pos ann) = return $ Syntax.RET pos (_convert_annotation ann)
_convertInstr (CDQ pos ann) = return $ Syntax.CDQ pos (_convert_annotation ann)
_convertInstr (CALL pos l ann) = return $ Syntax.CALL pos (Syntax.Label $ sanitizeLabel l) (_convert_annotation ann)
_convertInstr (CALL_INDIRECT pos RAX offset ann) = return $ Syntax.CALLINDIRECT pos offset (Syntax.RAX pos) (_convert_annotation ann)
_convertInstr (CALL_INDIRECT pos RBX offset ann) = return $ Syntax.CALLINDIRECT pos offset (Syntax.RBX pos) (_convert_annotation ann)
_convertInstr (CALL_INDIRECT pos RCX offset ann) = return $ Syntax.CALLINDIRECT pos offset (Syntax.RCX pos) (_convert_annotation ann)
_convertInstr (CALL_INDIRECT pos RDX offset ann) = return $ Syntax.CALLINDIRECT pos offset (Syntax.RDX pos) (_convert_annotation ann)
_convertInstr (CALL_INDIRECT pos RDI offset ann) = return $ Syntax.CALLINDIRECT pos offset (Syntax.RDI pos) (_convert_annotation ann)
_convertInstr (CALL_INDIRECT pos RSI offset ann) = return $ Syntax.CALLINDIRECT pos offset (Syntax.RSI pos) (_convert_annotation ann)
_convertInstr (CALL_INDIRECT pos RSP offset ann) = return $ Syntax.CALLINDIRECT pos offset (Syntax.RSP pos) (_convert_annotation ann)
_convertInstr (CALL_INDIRECT pos RBP offset ann) = return $ Syntax.CALLINDIRECT pos offset (Syntax.RBP pos) (_convert_annotation ann)
_convertInstr (CALL_INDIRECT pos R8 offset ann) = return $ Syntax.CALLINDIRECT pos offset (Syntax.R8 pos) (_convert_annotation ann)
_convertInstr (CALL_INDIRECT pos R9 offset ann) = return $ Syntax.CALLINDIRECT pos offset (Syntax.R9 pos) (_convert_annotation ann)
_convertInstr (CALL_INDIRECT pos R10 offset ann) = return $ Syntax.CALLINDIRECT pos offset (Syntax.R10 pos) (_convert_annotation ann)
_convertInstr (CALL_INDIRECT pos R11 offset ann) = return $ Syntax.CALLINDIRECT pos offset (Syntax.R11 pos) (_convert_annotation ann)
_convertInstr (CALL_INDIRECT pos R12 offset ann) = return $ Syntax.CALLINDIRECT pos offset (Syntax.R12 pos) (_convert_annotation ann)
_convertInstr (CALL_INDIRECT pos R13 offset ann) = return $ Syntax.CALLINDIRECT pos offset (Syntax.R13 pos) (_convert_annotation ann)
_convertInstr (CALL_INDIRECT pos R14 offset ann) = return $ Syntax.CALLINDIRECT pos offset (Syntax.R14 pos) (_convert_annotation ann)
_convertInstr (CALL_INDIRECT pos R15 offset ann) = return $ Syntax.CALLINDIRECT pos offset (Syntax.R15 pos) (_convert_annotation ann)

-- Data wrappers
dataDef :: (Monad m) => a -> DataDef -> ASMGeneratorT a anno m ()
dataDef pos def = _emitDef pos $ convertDataDef pos def

bssDef :: (Monad m) => a -> DataDef -> ASMGeneratorT a anno m ()
bssDef pos def = _emitBSS pos $ convertDataDef pos def
