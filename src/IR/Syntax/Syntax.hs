{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE InstanceSigs #-}
module IR.Syntax.Syntax where


import qualified Linearized.BuiltIns as BuiltIns

import           Data.Hashable
import           Data.Int
import           Data.Maybe    (fromJust)

newtype IRTargetRefName = IRTargetRefName String deriving (Eq, Ord, Show, Read)
newtype IRLabelName = IRLabelName String deriving (Eq, Ord, Show, Read)
newtype IRValueName = IRValueName String deriving (Eq, Ord, Show, Read)
data QIdent a = QIdent a IRTargetRefName IRTargetRefName
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor QIdent where
    fmap f x = case x of
        QIdent a symident1 symident2 -> QIdent (f a) symident1 symident2
data Program a = Program a (Metadata a) [Method a]
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor Program where
    fmap f x = case x of
        Program a metadata methods -> Program (f a) (fmap f metadata) (map (fmap f) methods)
data Metadata a = Meta a [ClassDef a]
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor Metadata where
    fmap f x = case x of
        Meta a classdefs -> Meta (f a) (map (fmap f) classdefs)
data ClassDef a = ClDef a IRTargetRefName [IRTargetRefName] [FieldDef a] [MethodDef a]
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor ClassDef where
    fmap f x = case x of
        ClDef a symident chain fielddefs methoddefs -> ClDef (f a) symident chain (map (fmap f) fielddefs) (map (fmap f) methoddefs)
data FieldDef a = FldDef a (SType a) IRTargetRefName
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor FieldDef where
    fmap f x = case x of
        FldDef a stype symident -> FldDef (f a) (fmap f stype) symident
data MethodDef a = MthdDef a String (FType a) (QIdent a)
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor MethodDef where
    fmap f x = case x of
        MthdDef a parName ftype qident -> MthdDef (f a) parName (fmap f ftype) (fmap f qident)
data FType a = FType a (SType a) [SType a]
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor FType where
    fmap f x = case x of
        FType a stype stypes -> FType (f a) (fmap f stype) (map (fmap f) stypes)
data SType a
    = Int a
    | Bool a
    | Void a
    | Arr a (SType a)
    | Cl a IRTargetRefName
    | Ref a (SType a)
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor SType where
    fmap f x = case x of
        Int a         -> Int (f a)
        Bool a        -> Bool (f a)
        Void a        -> Void (f a)
        Arr a stype   -> Arr (f a) (fmap f stype)
        Cl a symident -> Cl (f a) symident
        Ref a stype   -> Ref (f a) (fmap f stype)
data Method a = Mthd a (SType a) (QIdent a) [Param a] [Instr a]
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor Method where
    fmap f x = case x of
        Mthd a stype qident params instrs -> Mthd (f a) (fmap f stype) (fmap f qident) (map (fmap f) params) (map (fmap f) instrs)
data Param a = Param a (SType a) IRValueName
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor Param where
    fmap f x = case x of
        Param a stype valident -> Param (f a) (fmap f stype) valident
data Instr a
    = ILabel a IRLabelName
    | ILabelAnn a IRLabelName Integer Integer
    | IVRet a
    | IRet a (Val a)
    | IOp a IRValueName (Val a) (Op a) (Val a)
    | ISet a IRValueName (Val a)
    | ISwap a (SType a) IRValueName IRValueName
    | IUnOp a IRValueName (UnOp a) (Val a)
    | IVCall a (Call a)
    | ICall a IRValueName (Call a)
    | INew a IRValueName (SType a)
    | INewArr a IRValueName (SType a) (Val a)
    | INewStr a IRValueName String
    | IJmp a IRLabelName
    | ICondJmp a (Val a) IRLabelName IRLabelName
    | ILoad a IRValueName (Ptr a)
    | IStore a (Val a) (Ptr a)
    | IPhi a IRValueName [PhiVariant a]
    | IEndPhi a
    | IAddRef a (SType a) (Val a) Integer
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor Instr where
    fmap f x = case x of
        ILabel a labident -> ILabel (f a) labident
        ILabelAnn a labident integer1 integer2 -> ILabelAnn (f a) labident integer1 integer2
        IVRet a -> IVRet (f a)
        IRet a val -> IRet (f a) (fmap f val)
        IOp a valident val1 op val2 -> IOp (f a) valident (fmap f val1) (fmap f op) (fmap f val2)
        ISet a valident val -> ISet (f a) valident (fmap f val)
        ISwap a stype valident1 valident2 -> ISwap (f a) (fmap f stype) valident1 valident2
        IAddRef a stype val cnt -> IAddRef (f a) (fmap f stype) (fmap f val) cnt
        IUnOp a valident unop val -> IUnOp (f a) valident (fmap f unop) (fmap f val)
        IVCall a call -> IVCall (f a) (fmap f call)
        ICall a valident call -> ICall (f a) valident (fmap f call)
        INew a valident stype -> INew (f a) valident (fmap f stype)
        INewArr a valident stype val -> INewArr (f a) valident (fmap f stype) (fmap f val)
        INewStr a valident string -> INewStr (f a) valident string
        IJmp a labident -> IJmp (f a) labident
        ICondJmp a val labident1 labident2 -> ICondJmp (f a) (fmap f val) labident1 labident2
        ILoad a valident ptr -> ILoad (f a) valident (fmap f ptr)
        IStore a val ptr -> IStore (f a) (fmap f val) (fmap f ptr)
        IPhi a valident phivariants -> IPhi (f a) valident (map (fmap f) phivariants)
        IEndPhi a -> IEndPhi (f a)
data Ptr a
    = PFld a (SType a) (Val a) (QIdent a)
    | PElem a (SType a) (Val a) (Val a)
    | PArrLen a (Val a)
    | PLocal a (SType a) Integer
    | PParam a (SType a) Integer IRValueName
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor Ptr where
    fmap f x = case x of
        PFld a stype val qident -> PFld (f a) (fmap f stype) (fmap f val) (fmap f qident)
        PElem a stype val1 val2 -> PElem (f a) (fmap f stype) (fmap f val1) (fmap f val2)
        PArrLen a val -> PArrLen (f a) (fmap f val)
        PLocal a stype integer -> PLocal (f a) (fmap f stype) integer
        PParam a stype integer valident -> PParam (f a) (fmap f stype) integer valident
data PhiVariant a = PhiVar a IRLabelName (Val a)
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor PhiVariant where
    fmap f x = case x of
        PhiVar a labident val -> PhiVar (f a) labident (fmap f val)
data Call a
    = Call a (SType a) (QIdent a) [Val a] [IRLabelName]
    | CallVirt a (SType a) (QIdent a) [Val a]
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor Call where
    fmap f x = case x of
        Call a stype qident vals labs -> Call (f a) (fmap f stype) (fmap f qident) (map (fmap f) vals) labs
        CallVirt a stype qident vals -> CallVirt (f a) (fmap f stype) (fmap f qident) (map (fmap f) vals)
data Val a
    = VInt a Integer
    | VNegInt a Integer
    | VTrue a
    | VFalse a
    | VNull a (SType a)
    | VVal a (SType a) IRValueName
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor Val where
    fmap f x = case x of
        VInt a integer        -> VInt (f a) integer
        VNegInt a integer     -> VNegInt (f a) integer
        VTrue a               -> VTrue (f a)
        VFalse a              -> VFalse (f a)
        VNull a stype         -> VNull (f a) (fmap f stype)
        VVal a stype valident -> VVal (f a) (fmap f stype) valident
data Op a
    = OpAdd a
    | OpSub a
    | OpMul a
    | OpDiv a
    | OpMod a
    | OpLTH a
    | OpLE a
    | OpGTH a
    | OpGE a
    | OpEQU a
    | OpNE a
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor Op where
    fmap f x = case x of
        OpAdd a -> OpAdd (f a)
        OpSub a -> OpSub (f a)
        OpMul a -> OpMul (f a)
        OpDiv a -> OpDiv (f a)
        OpMod a -> OpMod (f a)
        OpLTH a -> OpLTH (f a)
        OpLE a  -> OpLE (f a)
        OpGTH a -> OpGTH (f a)
        OpGE a  -> OpGE (f a)
        OpEQU a -> OpEQU (f a)
        OpNE a  -> OpNE (f a)
data UnOp a = UnOpNeg a | UnOpNot a
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor UnOp where
    fmap f x = case x of
        UnOpNeg a -> UnOpNeg (f a)
        UnOpNot a -> UnOpNot (f a)

isLabel :: Instr a -> Bool
isLabel instr = case instr of
  ILabel {}    -> True
  ILabelAnn {} -> True
  _            -> False

isPhi :: Instr a -> Bool
isPhi instr = case instr of
    IPhi {} -> True
    _       -> False

isEndPhi :: Instr a -> Bool
isEndPhi instr = case instr of
    IEndPhi {} -> True
    _          -> False

instance Eq a => Hashable (Val a) where
    hashWithSalt salt val = case val of
        VInt _ n               -> hashWithSalt salt (0 :: Int8, n)
        VNegInt _ n            -> hashWithSalt salt (0 :: Int8, -n)
        VTrue _                -> hashWithSalt salt (1 :: Int8, True)
        VFalse _               -> hashWithSalt salt (1 :: Int8, False)
        VNull _ _              -> hashWithSalt salt (2 :: Int8, False)
        VVal _ _ (IRValueName vi) -> hashWithSalt salt (3 :: Int8, vi)

instance Eq a => Hashable (SType a) where
  hashWithSalt salt val = case val of
      Int _   -> hashWithSalt salt (0 :: Int8, 0 :: Int)
      Bool _  -> hashWithSalt salt (1 :: Int8, 1 :: Int)
      Void _  -> hashWithSalt salt (2 :: Int8, 2 :: Int)
      Arr _ t -> hashWithSalt salt (3 :: Int8, hashWithSalt salt t)
      Cl _ i  -> hashWithSalt salt (4 :: Int8, hashWithSalt salt i)
      Ref _ t -> hashWithSalt salt (5 :: Int8, hashWithSalt salt t)

instance Eq a => Hashable (Op a) where
    hashWithSalt salt val = case val of
        OpAdd _ -> hashWithSalt salt (0 :: Int8)
        OpSub _ -> hashWithSalt salt (1 :: Int8)
        OpMul _ -> hashWithSalt salt (2 :: Int8)
        OpDiv _ -> hashWithSalt salt (3 :: Int8)
        OpMod _ -> hashWithSalt salt (4 :: Int8)
        OpLTH _ -> hashWithSalt salt (5 :: Int8)
        OpLE _  -> hashWithSalt salt (6 :: Int8)
        OpGTH _ -> hashWithSalt salt (7 :: Int8)
        OpGE _  -> hashWithSalt salt (8 :: Int8)
        OpEQU _ -> hashWithSalt salt (9 :: Int8)
        OpNE _  -> hashWithSalt salt (10 :: Int8)

instance Eq a => Hashable (UnOp a) where
  hashWithSalt salt val = case val of
    UnOpNeg _ -> hashWithSalt salt False
    UnOpNot _ -> hashWithSalt salt True

instance Hashable IRTargetRefName where
  hashWithSalt :: Int -> IRTargetRefName -> Int
  hashWithSalt salt (IRTargetRefName s) = hashWithSalt salt s

instance Hashable IRValueName where
  hashWithSalt salt (IRValueName s) = hashWithSalt salt s


class ToString a where
    toStr :: a -> String

instance ToString IRLabelName where
    toStr (IRLabelName s) = s

instance ToString IRTargetRefName where
    toStr (IRTargetRefName s) = s

instance ToString IRValueName where
    toStr (IRValueName s) = s

argIRValueName :: String -> IRValueName
argIRValueName s = IRValueName $ "%a_" ++ s


constIdent :: String -> String
constIdent = ("__const_" ++)


entryLabel :: IRLabelName
entryLabel = IRLabelName ".L_entry"

exitLabel :: IRLabelName
exitLabel = IRLabelName ".L_exit"


indexedIRValueName :: String -> Integer -> IRValueName
indexedIRValueName i idx =
    let suf = if idx == 0 then "" else '_':show idx
    in valIdent (i ++ suf)

labIdent :: String -> IRLabelName
labIdent = IRLabelName . (".L_" ++)

labelFor :: QIdent a -> IRLabelName -> IRLabelName
labelFor (QIdent _ (IRTargetRefName i1) (IRTargetRefName i2)) (IRLabelName l1) = IRLabelName $ i1 ++ "." ++ i2 ++ l1

phiUnfoldJumpFromToLabel :: IRLabelName -> IRLabelName -> IRLabelName
phiUnfoldJumpFromToLabel (IRLabelName from) (IRLabelName to) = IRLabelName $ to ++ "__from_" ++ trim from
    where
        trim ('.':'L':'_':xs) = xs
        trim xs               = xs


sanitiseAssembly :: String -> String
sanitiseAssembly s = case s of
    []     -> []
    '~':xs -> '_':'_':sanitiseAssembly xs
    x:xs   -> x:sanitiseAssembly xs


nullrefLabel :: IRLabelName
nullrefLabel = IRLabelName "__handleErrorNull"


valIdent :: String -> IRValueName
valIdent = IRValueName . ("%v_" ++)

classDefIdent :: IRTargetRefName -> IRLabelName
classDefIdent (IRTargetRefName i) =  IRLabelName $ "_class_" ++ i

vTableIRLabelName :: IRTargetRefName -> IRLabelName
vTableIRLabelName (IRTargetRefName i) =  IRLabelName $ "_class_" ++ i ++ "_methods"

runtimeSymbols :: [String]
runtimeSymbols = map (\(_, (n, _, _)) -> n) BuiltIns.builtInsLabels

