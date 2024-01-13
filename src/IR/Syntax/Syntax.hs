{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE InstanceSigs #-}
module IR.Syntax.Syntax where

import           Data.Hashable
import           Data.Int
import           Data.Maybe    (fromJust)

-- Haskell module generated by the BNF converter

newtype SymIdent = SymIdent String deriving (Eq, Ord, Show, Read)
newtype LabIdent = LabIdent String deriving (Eq, Ord, Show, Read)
newtype ValIdent = ValIdent String deriving (Eq, Ord, Show, Read)
data QIdent a = QIdent a SymIdent SymIdent
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
data ClassDef a = ClDef a SymIdent [FieldDef a] [MethodDef a]
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor ClassDef where
    fmap f x = case x of
        ClDef a symident fielddefs methoddefs -> ClDef (f a) symident (map (fmap f) fielddefs) (map (fmap f) methoddefs)
data FieldDef a = FldDef a (SType a) SymIdent
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor FieldDef where
    fmap f x = case x of
        FldDef a stype symident -> FldDef (f a) (fmap f stype) symident
data MethodDef a = MthdDef a (FType a) (QIdent a)
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor MethodDef where
    fmap f x = case x of
        MthdDef a ftype qident -> MthdDef (f a) (fmap f ftype) (fmap f qident)
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
    | Cl a SymIdent
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
data Param a = Param a (SType a) ValIdent
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor Param where
    fmap f x = case x of
        Param a stype valident -> Param (f a) (fmap f stype) valident
data Instr a
    = ILabel a LabIdent
    | ILabelAnn a LabIdent Integer Integer
    | IVRet a
    | IRet a (Val a)
    | IOp a ValIdent (Val a) (Op a) (Val a)
    | ISet a ValIdent (Val a)
    | ISwap a (SType a) ValIdent ValIdent
    | IUnOp a ValIdent (UnOp a) (Val a)
    | IVCall a (Call a)
    | ICall a ValIdent (Call a)
    | INew a ValIdent (SType a)
    | INewArr a ValIdent (SType a) (Val a)
    | INewStr a ValIdent String
    | IJmp a LabIdent
    | ICondJmp a (Val a) LabIdent LabIdent
    | ILoad a ValIdent (Ptr a)
    | IStore a (Val a) (Ptr a)
    | IPhi a ValIdent [PhiVariant a]
    | IEndPhi a
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
    | PParam a (SType a) Integer ValIdent
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor Ptr where
    fmap f x = case x of
        PFld a stype val qident -> PFld (f a) (fmap f stype) (fmap f val) (fmap f qident)
        PElem a stype val1 val2 -> PElem (f a) (fmap f stype) (fmap f val1) (fmap f val2)
        PArrLen a val -> PArrLen (f a) (fmap f val)
        PLocal a stype integer -> PLocal (f a) (fmap f stype) integer
        PParam a stype integer valident -> PParam (f a) (fmap f stype) integer valident
data PhiVariant a = PhiVar a LabIdent (Val a)
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor PhiVariant where
    fmap f x = case x of
        PhiVar a labident val -> PhiVar (f a) labident (fmap f val)
data Call a
    = Call a (SType a) (QIdent a) [Val a]
    | CallVirt a (SType a) (QIdent a) [Val a]
  deriving (Eq, Ord, Show, Read, Foldable)

instance Functor Call where
    fmap f x = case x of
        Call a stype qident vals -> Call (f a) (fmap f stype) (fmap f qident) (map (fmap f) vals)
        CallVirt a stype qident vals -> CallVirt (f a) (fmap f stype) (fmap f qident) (map (fmap f) vals)
data Val a
    = VInt a Integer
    | VNegInt a Integer
    | VTrue a
    | VFalse a
    | VNull a (SType a)
    | VVal a (SType a) ValIdent
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
        VVal _ _ (ValIdent vi) -> hashWithSalt salt (3 :: Int8, vi)

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

instance Hashable SymIdent where
  hashWithSalt :: Int -> SymIdent -> Int
  hashWithSalt salt (SymIdent s) = hashWithSalt salt s

instance Hashable ValIdent where
  hashWithSalt salt (ValIdent s) = hashWithSalt salt s

--- UTILS ---


class ToString a where
    toStr :: a -> String

instance ToString LabIdent where
    toStr (LabIdent s) = s

instance ToString SymIdent where
    toStr (SymIdent s) = s

instance ToString ValIdent where
    toStr (ValIdent s) = s

argValIdent :: String -> ValIdent
argValIdent s = ValIdent $ "%a_" ++ s


constIdent :: String -> String
constIdent = ("__const_" ++)


entryLabel :: LabIdent
entryLabel = LabIdent ".L_entry"

exitLabel :: LabIdent
exitLabel = LabIdent ".L_exit"


indexedValIdent :: String -> Integer -> ValIdent
indexedValIdent i idx =
    let suf = if idx == 0 then "" else '_':show idx
    in valIdent (i ++ suf)

labIdent :: String -> LabIdent
labIdent = LabIdent . (".L_" ++)

labelFor :: QIdent a -> LabIdent -> LabIdent
labelFor (QIdent _ (SymIdent i1) (SymIdent i2)) (LabIdent l1) = LabIdent $ i1 ++ "." ++ i2 ++ l1

phiUnfoldJumpFromToLabel :: LabIdent -> LabIdent -> LabIdent
phiUnfoldJumpFromToLabel (LabIdent from) (LabIdent to) = LabIdent $ to ++ "__from_" ++ trim from
    where
        trim ('.':'L':'_':xs) = xs
        trim xs               = xs


sanitiseAssembly :: String -> String
sanitiseAssembly s = case s of
    []     -> []
    '~':xs -> '_':'_':sanitiseAssembly xs
    x:xs   -> x:sanitiseAssembly xs


nullrefLabel :: LabIdent
nullrefLabel = LabIdent "__nullref"


valIdent :: String -> ValIdent
valIdent = ValIdent . ("%v_" ++)

vTableLabIdent :: SymIdent -> LabIdent
vTableLabIdent (SymIdent i) =  LabIdent $ "__vtable_" ++ i

runtimeSymbols :: [String]
runtimeSymbols = [
        "lat_print_int",
        "lat_print_string",
        "lat_read_int",
        "lat_read_string",
        "lat_error",
        "lat_nullref",
        "lat_new_string",
        "lat_cat_strings"
    ]


--- END UTILS ---