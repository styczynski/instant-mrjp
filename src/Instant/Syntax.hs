{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}

module Instant.Syntax where

import Control.Applicative (liftA2)
import Control.Monad
import Control.Monad.Identity
import Data.Bifunctor
import Data.List as DL
import Data.Map (Map)
import Data.String
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NEL
import GHC.TypeNats(Nat, type (+))
import Control.Lens
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (EQ, GT, LT, (~))


data Lit
  = LInt Integer
  | LString String
  | LBool Bool
  deriving (Show)


newtype VarId = VarId {_vidIdStr :: String} deriving (Eq, Ord, Show)
newtype FunId = FunId {_fnidIdStr :: String} deriving (Eq, Ord, Show)
newtype ClassId = ClassId {_clidIdStr :: String} deriving (Eq, Ord, Show)
newtype MethodId = MethodId {_midIdStr :: String} deriving (Eq, Ord, Show)
newtype FieldId = FieldId {_flidIdStr :: String} deriving (Eq, Ord, Show)
newtype ConstructorId = ConstructorId {_ctidIdStr :: String} deriving (Eq, Ord, Show)

data ASTMeta = ASTMeta {line :: Int, column :: Int, file :: String}
  deriving (Eq, Show)

emptyMeta :: ASTMeta
emptyMeta = ASTMeta 0 0 "<no location>"

class Meta a where
  getMetaDescription :: a -> String

instance Meta ASTMeta where
  getMetaDescription astMeta = file astMeta ++ ":" ++ show (line astMeta) ++ ":" ++ show (column astMeta)


block :: String -> String
block b = "{" ++ b ++ "}"

data Type
  = TVoid
  | TInt
  | TBool
  | TString
  | TClass ClassId
  deriving (Eq)


data Arg = Arg
  { _argAnn :: ASTMeta
  , _argTy :: Type
  , _argName :: VarId
  }


type Args = [Arg]


data OpType = Rel | Add | Mul | Log

data Op (t :: OpType) where
  LT    :: ASTMeta -> Op 'Rel
  LEQ   :: ASTMeta -> Op 'Rel
  EQ    :: ASTMeta -> Op 'Rel
  NEQ   :: ASTMeta -> Op 'Rel
  GEQ   :: ASTMeta -> Op 'Rel
  GT    :: ASTMeta -> Op 'Rel
  Plus  :: ASTMeta -> Op 'Add
  Minus :: ASTMeta -> Op 'Add
  Mult  :: ASTMeta -> Op 'Mul
  Div   :: ASTMeta -> Op 'Mul
  Mod   :: ASTMeta -> Op 'Mul
  Or    :: ASTMeta -> Op 'Log
  And   :: ASTMeta -> Op 'Log
deriving instance Eq (Op t)


data AnyOp where
  Op :: Op t -> AnyOp


data UnOp
  = Neg
  | Not


data ASTNodeChild (l :: Nat) where
  REOr    :: ASTMeta -> ASTNodeChild 1 -> ASTNodeChild 0                  -> ASTNodeChild 0
  REAnd   :: ASTMeta -> ASTNodeChild 2 -> ASTNodeChild 1                  -> ASTNodeChild 1
  RERelOp :: ASTMeta -> Op 'Rel -> ASTNodeChild 2 -> ASTNodeChild 3       -> ASTNodeChild 2
  REAddOp :: ASTMeta -> Op 'Add -> ASTNodeChild 3 -> ASTNodeChild 4       -> ASTNodeChild 3
  REMulOp :: ASTMeta -> Op 'Mul -> ASTNodeChild 4 -> ASTNodeChild 5       -> ASTNodeChild 4
  RENot   :: ASTMeta -> ASTNodeChild 6                               -> ASTNodeChild 5
  RENeg   :: ASTMeta -> ASTNodeChild 6                               -> ASTNodeChild 5
  REProj  :: ASTMeta -> ASTNodeChild 6 -> FieldId                    -> ASTNodeChild 6
  REMApp  :: ASTMeta -> ASTNodeChild 6 -> MethodId -> [ASTNodeChild 0]    -> ASTNodeChild 6
  RELit   :: ASTMeta -> Lit                                     -> ASTNodeChild 7
  REApp   :: ASTMeta -> FunId   -> [ASTNodeChild 0]                  -> ASTNodeChild 7
  RENew   :: ASTMeta -> ClassId -> Maybe ConstructorId -> [ASTNodeChild 0] -> ASTNodeChild 7
  REVar   :: ASTMeta -> VarId                                   -> ASTNodeChild 7
  REPar   :: ASTMeta -> ASTNodeChild 0                               -> ASTNodeChild 7
  RESuper :: ASTMeta                                            -> ASTNodeChild 7
  RECoe   ::        ASTNodeChild (n + 1)                         -> ASTNodeChild n

type E = ASTNodeChild 0


data ASTNode
  = RSAssg ASTMeta VarId E
  | RSFieldAssg ASTMeta E FieldId E
  | RSDecl ASTMeta Type (NEL.NonEmpty (VarId, Maybe E))
  | RSIncr ASTMeta VarId
  | RSDecr ASTMeta VarId
  | RSRet ASTMeta E
  | RSVRet ASTMeta
  | RSCond ASTMeta E ASTNode
  | RSCondElse ASTMeta E ASTNode ASTNode
  | RSWhile ASTMeta E ASTNode
  | RSExp ASTMeta E
  | RSBlock ASTMeta [ASTNode]
  | RSEmpty ASTMeta


data Stage = Untyped | Typed


type family ExprDecoration (s :: Stage) where
  ExprDecoration 'Untyped = ()
  ExprDecoration 'Typed = Type


data Expr (s :: Stage) where
  ELit   :: ASTMeta -> ExprDecoration s -> Lit -> Expr s
  EApp   :: ASTMeta -> ExprDecoration s -> FunId -> [Expr s] -> Expr s
  EVar   :: ASTMeta -> ExprDecoration s -> VarId -> Expr s
  EUnOp  :: ASTMeta -> ExprDecoration s -> UnOp -> Expr s -> Expr s
  EOp    :: ASTMeta -> ExprDecoration s -> AnyOp -> Expr s -> Expr s -> Expr s
  EProj  :: ASTMeta -> ExprDecoration s -> Expr s -> FieldId -> Expr s
  EMApp  :: ASTMeta -> ExprDecoration s -> Expr s -> MethodId -> [Expr s] -> Expr s
  ENew   :: ASTMeta -> ExprDecoration s -> ClassId -> Maybe ConstructorId -> [Expr s] -> Expr s
  ESuper :: ASTMeta -> ExprDecoration s -> Expr s

getExprDec :: Expr s -> ExprDecoration s
getExprDec = \case
  ELit _ a _ -> a
  EVar _ a _ -> a
  EApp _ a _ _ -> a
  EUnOp _ a _ _ -> a
  EOp  _ a _ _ _ -> a
  EProj _ a _ _ -> a
  EMApp _ a _ _ _ -> a
  ENew _ a _ _ _ -> a
  ESuper _ a -> a


data Stmt (e :: Stage)
  = SAssg ASTMeta VarId (Expr e) (Stmt e)
  | SFieldAssg ASTMeta (Expr e) FieldId (Expr e) (Stmt e)
  | SDecl ASTMeta Type [(VarId, (Maybe (Expr e)))] (Stmt e)
  | SIncr ASTMeta VarId (Stmt e)
  | SDecr ASTMeta VarId (Stmt e)
  | SRet ASTMeta (Expr e) (Stmt e)
  | SVRet ASTMeta (Stmt e)
  | SCond ASTMeta (Expr e) (Stmt e) (Stmt e)
  | SCondElse ASTMeta (Expr e) (Stmt e) (Stmt e) (Stmt e)
  | SWhile ASTMeta (Expr e) (Stmt e) (Stmt e)
  | SExp ASTMeta (Expr e) (Stmt e)
  | SBlock ASTMeta (Stmt e) (Stmt e)
  | SEmpty ASTMeta


data FunDef (s :: Stage) = FunDef
  { _fundefMeta :: ASTMeta
  , _fundefRetType :: Type
  , _fundefName :: FunId
  , _fundefArgs :: [Arg]
  , _fundefBody :: Stmt s
  }


data ClassDef (s :: Stage) = ClassDef
  { _classdefMeta :: ASTMeta
  , _classdefName :: ClassId
  , _classdefSuper :: Maybe ClassId
  , _classdefBody :: [ClassMember s]
  }


data TopDef (s :: Stage)
  = TDFun (FunDef s)
  | TDClass (ClassDef s)

data Method (s :: Stage) = Method
  { _methodMeta :: ASTMeta
  , _methodAccess :: ClassMemberAccess
  , _methodPlace :: ClassMemberPlace
  , _methodRetType :: Type
  , _methodName :: MethodId
  , _methodArgs :: [Arg]
  , _methodBody :: Maybe (Stmt s)
  }


data Field (s :: Stage) = Field
  { _fieldMeta :: ASTMeta
  , _fieldAccess :: ClassMemberAccess
  , _fieldPlace :: ClassMemberPlace
  , _fieldTy :: Type
  , _fieldAssignments :: NEL.NonEmpty (FieldId, Maybe (Expr s))
  }


data Constructor (s :: Stage) = Constructor
  { _constructorMeta :: ASTMeta
  , _constructorAccess :: ClassMemberAccess
  , _constructorName :: Maybe ConstructorId
  , _constructorArgs :: [Arg]
  , _constructorBody :: Stmt s
  }


data ClassMember (s :: Stage)
  = CMMethod (Method s)
  | CMField (Field s)
  | CMConstructor (Constructor s)


data ClassMemberPlace = Dynamic | Static
  deriving (Show)


data ClassMemberAccess = Private | Public | Protected
  deriving (Show)


makeLensesWith abbreviatedFields ''VarId
makeLensesWith abbreviatedFields ''FunId
makeLensesWith abbreviatedFields ''ClassId
makeLensesWith abbreviatedFields ''FieldId
makeLensesWith abbreviatedFields ''MethodId
makeLensesWith abbreviatedFields ''ConstructorId
makeLensesWith abbreviatedFields ''ASTMeta
makeLensesWith abbreviatedFields ''Arg
makeLensesWith abbreviatedFields ''Type
makeLensesWith abbreviatedFields ''FunDef
makeLensesWith abbreviatedFields ''ClassDef
makeLensesWith abbreviatedFields ''Method
makeLensesWith abbreviatedFields ''Field
makeLensesWith abbreviatedFields ''Constructor


class (HasIdStr a String, Ord a, Eq a) => IsId a where
instance (HasIdStr a String, Ord a, Eq a) => IsId a where

instance IsString VarId where
  fromString = VarId

instance IsString FunId where
  fromString = FunId

instance IsString ClassId where
  fromString = ClassId

instance IsString FieldId where
  fromString = FieldId

instance IsString MethodId where
  fromString = MethodId

instance IsString ConstructorId where
  fromString = ConstructorId

instance INodeTrivial VarId where
  -- fromAST node = node
  simplePretty i = "X"

instance INodeTrivial FunId where
  -- fromAST node = node
  simplePretty i = "X"

instance INodeTrivial ClassId where
  -- fromAST node = node
  simplePretty i = "X"

instance INodeTrivial FieldId where
  -- fromAST node = node
  simplePretty i = "X"

instance INodeTrivial MethodId where
  -- fromAST node = node
  simplePretty i = "X"

instance INodeTrivial ConstructorId where
  -- fromAST node = node
  simplePretty i = "X"


instance INodeTrivial Arg where
  -- fromAST node = node
  simplePretty (Arg _ t i) = simplePretty t ++ simplePretty i


instance INodeTrivial Lit where
  -- fromAST node = node
  simplePretty = \case
    LInt i -> show i
    LString s -> "\"" ++ s ++ "\""
    LBool True -> "true"
    LBool False -> "false"


instance INodeTrivial ASTMeta where
  -- fromAST node = node
  simplePretty astMeta = file astMeta ++ ":" ++ show (line astMeta) ++ ":" ++ show (column astMeta)
  -- pretty a = text (a^.file) <> ":" <> int (a^.line) <> ":" <> int (a^.column)


instance INodeTrivial (Op t) where
  -- fromAST node = node
  simplePretty = \case
    LT _    -> "<"
    LEQ _   -> "<="
    EQ _    -> "=="
    NEQ _   -> "!="
    GEQ _   -> ">="
    GT _    -> ">"
    Plus _  -> "+"
    Minus _ -> "-"
    Mult _  -> "*"
    Div _   -> "/"
    Mod _   -> "%"
    Or _    -> "||"
    And _   -> "&&"


instance INodeTrivial AnyOp where
  -- fromAST node = node
  simplePretty (Op o) = simplePretty o


instance INodeTrivial UnOp where
  -- fromAST node = node
  simplePretty = \case
    Neg -> "-"
    Not -> "!"


instance INodeTrivial Type where
  -- fromAST node = node
  simplePretty = \case
    TVoid -> "void"
    TInt -> "int"
    TBool -> "boolean"
    TString -> "string"
    TClass i -> simplePretty i


instance INode (ASTNodeChild t) (Expr 'Untyped) where
  fromAST = \case
    REOr    a e1 e2 -> EOp a () (Op $ Or a)  (fromAST e1) (fromAST e2)
    REAnd   a e1 e2 -> EOp a () (Op $ And a) (fromAST e1) (fromAST e2)
    RERelOp a o e1 e2 -> EOp a () (Op o) (fromAST e1) (fromAST e2)
    REAddOp a o e1 e2 -> EOp a () (Op o) (fromAST e1) (fromAST e2)
    REMulOp a o e1 e2 -> EOp a () (Op o) (fromAST e1) (fromAST e2)
    RENeg   a e -> EUnOp a () Neg (fromAST e)
    RENot   a e -> EUnOp a () Not (fromAST e)
    RELit   a l -> ELit a () l
    REApp   a f as -> EApp a () f (map fromAST as)
    RENew   a c f as -> ENew a () c f (map fromAST as)
    REVar   a v -> EVar a () v
    REPar   _ e -> fromAST e
    RECoe   e -> fromAST e
    REProj  a e i -> EProj a () (fromAST e) i
    REMApp  a e i as -> EMApp a () (fromAST e) i (map fromAST as)
    RESuper a -> ESuper a ()
  pretty _ = "EXPR"
  -- pretty k = \case
  --   ELit _ _ l -> pretty l
  --   EApp _ _ f as -> pretty f <> parens (cat $ punctuate comma $ map pretty as)
  --   EVar _ _ v -> pretty v
  --   EUnOp _ _ o e -> pretty o <> pretty e
  --   EOp  _ _ o l r ->
  --     let pri :: AnyOp -> Rational
  --         pri = \case
  --           Op (Plus _)  -> 6
  --           Op (Minus _) -> 6
  --           Op (Mult _)  -> 7
  --           Op (Div _)   -> 7
  --           Op (Mod _)   -> 7
  --           Op (LT _)    -> 8
  --           Op (LEQ _)   -> 8
  --           Op (EQ _)    -> 8
  --           Op (NEQ _)   -> 8
  --           Op (GEQ _)   -> 8
  --           Op (GT _)    -> 8
  --           Op (And _)   -> 9
  --           Op (Or _)    -> 10
  --     in (pretty l) ++ (pri o) ++ (pretty r)
  --   EProj _ _ e i -> pretty e <> "." <> pretty i
  --   EMApp _ _ e m as -> pretty e <> "." <> pretty m <>
  --                          parens (cat $ punctuate comma $ map pretty as)
  --   ENew _ _ cl cr as -> "new" <+> pretty cl <> maybe empty (\n -> "." <> pretty n) cr <>
  --     parens (cat $ punctuate comma $ map pretty as)
  --   ESuper _ _ -> "super"



instance INode (ASTNode) (Stmt 'Untyped) where
  fromAST s = entailSingle s (SEmpty emptyMeta)
    where
      entailSingle :: ASTNode -> Stmt 'Untyped -> Stmt 'Untyped
      entailSingle = \case
        RSAssg a i v -> SAssg a i (fromAST v)
        RSFieldAssg a e i v -> SFieldAssg a (fromAST e) i (fromAST v)
        RSDecl a t vs -> SDecl a t $ flip fmap (NEL.toList vs) $ \(i, mval) -> (i, fmap fromAST mval)
        RSIncr a i -> SIncr a i
        RSDecr a i -> SDecr a i
        RSRet a e -> SRet a (fromAST e)
        RSVRet a -> SVRet a
        RSCond a c t -> SCond a (fromAST c) (fromAST t)
        RSCondElse a c t e -> SCondElse a (fromAST c) (fromAST t) (fromAST e)
        RSWhile a c b -> SWhile a (fromAST c) (fromAST b)
        RSExp a e -> SExp a (fromAST e)
        RSBlock a sts -> \k ->
          let composed = (composeSts $ map entailSingle sts) in case k of
            SEmpty _ -> composed
            _        -> SBlock a composed k
        RSEmpty _ -> id
      composeSts :: [Stmt 'Untyped -> Stmt 'Untyped] -> Stmt 'Untyped
      composeSts fs = foldr (.) id fs $ SEmpty emptyMeta
  pretty _ = "STATEMENT"
  -- pretty = \case
  --   SAssg _ v e cont -> pretty v <+> "=" <+> pretty e <> semi $+$ pretty cont
  --   SFieldAssg _ b v e cont -> pretty b <> "." <> pretty v <+> "=" <+> pretty e <> semi $+$ pretty cont
  --   SDecl _ t v cont -> pretty t <+>
  --     vcat (punctuate comma $ flip fmap v $ \(var, mval) -> case mval of
  --              Nothing -> pretty var
  --              Just val  -> pretty var <+> "=" <+> pretty val
  --          ) <> semi $+$ pretty cont
  --   SIncr _ v cont -> pretty v <> "++" <> semi $+$ pretty cont
  --   SDecr _ v cont -> pretty v <> "--" <> semi $+$ pretty cont
  --   SRet _ e cont -> "return" <+> pretty e <> semi $+$ pretty cont
  --   SVRet _ cont -> "return" <> semi $+$ pretty cont
  --   SCond _ c t cont -> "if" <> parens (pretty c) <+> block (pretty t) $+$ pretty cont
  --   SCondElse _ c t e cont -> "if" <> parens (pretty c) <+> block (pretty t) $+$
  --                             "else" <+> block (pretty e) $+$
  --                             pretty cont
  --   SWhile _ c b cont -> "while" <> parens (pretty c) <+> block (pretty b) $+$ pretty cont
  --   SExp _ e cont -> pretty e $+$ pretty cont
  --   SBlock _ b cont -> block (pretty b) $+$ pretty cont
  --   SEmpty _ -> empty


instance INodeTrivial (FunDef a) where
  -- fromAST node = node
  simplePretty _ = "FUN_DEF"
  -- pretty fd = pretty (fd^.retType) <+> pretty (fd^.name) <> parens (cat $ punctuate comma $ map pretty $ fd^.args) <+>
  --             block (pretty (fd^.body))


instance INodeTrivial (TopDef a) where
  -- fromAST node = node
  simplePretty = \case
    TDFun f -> simplePretty f
    TDClass c -> simplePretty c


instance INodeTrivial (ClassDef a) where
  simplePretty _ = "CLASS_DEF"
  -- fromAST node = node
  -- pretty c = "class" <+> pretty (c^.name) <+> maybe empty (("extends" <+>) . pretty) (c^.super) <+>
  --   block ( vcat $ map pretty (c^.body))


instance INodeTrivial (ClassMember a) where
  -- fromAST node = node
  simplePretty = \case
    CMMethod m -> simplePretty m
    CMField f -> simplePretty f
    CMConstructor c -> simplePretty c


instance INodeTrivial (Method a) where
  -- fromAST node = node
  simplePretty _ = "METHOD"
  -- pretty md =
  --   pretty (md^.place) <+> pretty (md^.access) <+>
  --   pretty (md^.retType) <+> pretty (md^.name) <>
  --   parens (cat $ punctuate comma $ map pretty $ md^.args) <+>
  --   block (maybe empty pretty (md^.body))


instance INodeTrivial (Field a) where
  -- fromAST node = node
  simplePretty _ = "FIELD"
  -- pretty f =
  --   pretty (f^.place) <+> pretty (f^.access) <+>
  --   pretty (f^.ty) <+>
  --   cat (punctuate comma (map (\(i, mv) ->
  --                                 pretty i <+> maybe empty (\v -> "=" <+> pretty v) mv)
  --                          (NEL.toList $ f^.assignments))) <>
  --   semi

instance INodeTrivial (Constructor a) where
  -- fromAST node = node
  simplePretty _ = "CONSTRUCTOR"
  -- pretty c =
  --   "new" <+>
  --   pretty (c^.name) <>
  --   parens (cat $ punctuate comma $ map pretty $ c^.args) <+>
  --   block (pretty (c^.body))

instance INodeTrivial ClassMemberPlace where
  -- fromAST node = node
  simplePretty = \case
    Dynamic -> ""
    Static -> "static"


instance INodeTrivial ClassMemberAccess where
  -- fromAST node = node
  simplePretty = \case
    Private   -> "private"
    Public    -> "public"
    Protected -> "protected"


newtype ICode = ICode {statements :: [TopDef 'Untyped]}

type InstantProgram = ICode

-- deriving instance Eq (ICode)
-- deriving instance Show (ICode)

class INode l a where
  fromAST :: l -> a
  pretty :: a -> String

class INodeTrivial l where
  simplePretty :: l -> String

instance {-# OVERLAPPABLE #-} (INodeTrivial l) => INode l l where
  pretty = simplePretty
  fromAST node = node

instance INode InstantProgram ICode where
  fromAST code = code
  pretty = foldMap ((<> "\n") . simplePretty) . statements

instance HasAnn (Expr p) ASTMeta where
  ann f = \case
    ELit a dec l -> fmap (\a2 -> ELit a2 dec l) (f a)
    EVar a dec v -> fmap (\a2 -> EVar a2 dec v) (f a)
    EApp a dec fname as -> fmap (\a2 -> EApp a2 dec fname as) (f a)
    EUnOp a dec o v -> fmap (\a2 -> EUnOp a2 dec o v) (f a)
    EOp a dec o l r -> fmap (\a2 -> EOp a2 dec o l r) (f a)
    EProj a dec e i -> fmap (\a2 -> EProj a2 dec e i) (f a)
    EMApp a dec e i as -> fmap (\a2 -> EMApp a2 dec e i as) (f a)
    ENew a dec c i as -> fmap (\a2 -> ENew a2 dec c i as) (f a)
    ESuper a dec -> fmap (\a2 -> ESuper a2 dec) (f a)