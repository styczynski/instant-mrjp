-- File generated by the BNF Converter (bnfc 2.9.4).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for IR.

module IR.Parser.Gen.PrintIR where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified IR.Parser.Gen.AbsIR

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print IR.Parser.Gen.AbsIR.SymIdent where
  prt _ (IR.Parser.Gen.AbsIR.SymIdent i) = doc $ showString i
instance Print IR.Parser.Gen.AbsIR.LabIdent where
  prt _ (IR.Parser.Gen.AbsIR.LabIdent i) = doc $ showString i
instance Print IR.Parser.Gen.AbsIR.ValIdent where
  prt _ (IR.Parser.Gen.AbsIR.ValIdent i) = doc $ showString i
instance Print (IR.Parser.Gen.AbsIR.QIdent' a) where
  prt i = \case
    IR.Parser.Gen.AbsIR.QIdent _ symident1 symident2 -> prPrec i 0 (concatD [prt 0 symident1, doc (showString "."), prt 0 symident2])

instance Print (IR.Parser.Gen.AbsIR.Program' a) where
  prt i = \case
    IR.Parser.Gen.AbsIR.Program _ metadata methods -> prPrec i 0 (concatD [prt 0 metadata, prt 0 methods])

instance Print (IR.Parser.Gen.AbsIR.Metadata' a) where
  prt i = \case
    IR.Parser.Gen.AbsIR.Meta _ classdefs -> prPrec i 0 (concatD [doc (showString ".metadata"), doc (showString ":"), doc (showString "["), doc (showString ".classes"), doc (showString ":"), doc (showString "["), prt 0 classdefs, doc (showString "]"), doc (showString "]")])

instance Print (IR.Parser.Gen.AbsIR.ClassDef' a) where
  prt i = \case
    IR.Parser.Gen.AbsIR.ClDef _ symident fielddefs methoddefs -> prPrec i 0 (concatD [prt 0 symident, doc (showString ":"), doc (showString "["), doc (showString ".fields"), doc (showString ":"), doc (showString "["), prt 0 fielddefs, doc (showString "]"), doc (showString ".methods"), doc (showString ":"), doc (showString "["), prt 0 methoddefs, doc (showString "]"), doc (showString "]")])

instance Print (IR.Parser.Gen.AbsIR.FieldDef' a) where
  prt i = \case
    IR.Parser.Gen.AbsIR.FldDef _ stype symident -> prPrec i 0 (concatD [prt 0 stype, prt 0 symident])

instance Print (IR.Parser.Gen.AbsIR.MethodDef' a) where
  prt i = \case
    IR.Parser.Gen.AbsIR.MthdDef _ ftype qident -> prPrec i 0 (concatD [prt 0 ftype, prt 0 qident])

instance Print [IR.Parser.Gen.AbsIR.FieldDef' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print [IR.Parser.Gen.AbsIR.MethodDef' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print [IR.Parser.Gen.AbsIR.ClassDef' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (IR.Parser.Gen.AbsIR.FType' a) where
  prt i = \case
    IR.Parser.Gen.AbsIR.FType _ stype stypes -> prPrec i 0 (concatD [prt 0 stype, doc (showString "("), prt 0 stypes, doc (showString ")")])

instance Print (IR.Parser.Gen.AbsIR.SType' a) where
  prt i = \case
    IR.Parser.Gen.AbsIR.Int _ -> prPrec i 0 (concatD [doc (showString "int")])
    IR.Parser.Gen.AbsIR.Bool _ -> prPrec i 0 (concatD [doc (showString "boolean")])
    IR.Parser.Gen.AbsIR.Void _ -> prPrec i 0 (concatD [doc (showString "void")])
    IR.Parser.Gen.AbsIR.Arr _ stype -> prPrec i 0 (concatD [prt 0 stype, doc (showString "[]")])
    IR.Parser.Gen.AbsIR.Cl _ symident -> prPrec i 0 (concatD [prt 0 symident])
    IR.Parser.Gen.AbsIR.Ref _ stype -> prPrec i 0 (concatD [prt 0 stype, doc (showString "&")])

instance Print [IR.Parser.Gen.AbsIR.SType' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (IR.Parser.Gen.AbsIR.Method' a) where
  prt i = \case
    IR.Parser.Gen.AbsIR.Mthd _ stype qident params instrs -> prPrec i 0 (concatD [doc (showString ".method"), prt 0 stype, prt 0 qident, doc (showString "("), prt 0 params, doc (showString ")"), doc (showString ":"), doc (showString "["), prt 0 instrs, doc (showString "]")])

instance Print (IR.Parser.Gen.AbsIR.Param' a) where
  prt i = \case
    IR.Parser.Gen.AbsIR.Param _ stype valident -> prPrec i 0 (concatD [prt 0 stype, prt 0 valident])

instance Print [IR.Parser.Gen.AbsIR.Param' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [IR.Parser.Gen.AbsIR.Method' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (IR.Parser.Gen.AbsIR.Instr' a) where
  prt i = \case
    IR.Parser.Gen.AbsIR.ILabel _ labident -> prPrec i 0 (concatD [prt 0 labident, doc (showString ":")])
    IR.Parser.Gen.AbsIR.ILabelAnn _ labident n1 n2 -> prPrec i 0 (concatD [prt 0 labident, doc (showString ":"), doc (showString "("), doc (showString "lines"), prt 0 n1, doc (showString "to"), prt 0 n2, doc (showString ")")])
    IR.Parser.Gen.AbsIR.IVRet _ -> prPrec i 0 (concatD [doc (showString "return"), doc (showString ";")])
    IR.Parser.Gen.AbsIR.IRet _ val -> prPrec i 0 (concatD [doc (showString "return"), prt 0 val, doc (showString ";")])
    IR.Parser.Gen.AbsIR.IOp _ valident val1 op val2 -> prPrec i 0 (concatD [prt 0 valident, doc (showString ":="), prt 0 val1, prt 0 op, prt 0 val2, doc (showString ";")])
    IR.Parser.Gen.AbsIR.ISet _ valident val -> prPrec i 0 (concatD [prt 0 valident, doc (showString ":="), prt 0 val, doc (showString ";")])
    IR.Parser.Gen.AbsIR.ISwap _ stype valident1 valident2 -> prPrec i 0 (concatD [doc (showString "swap"), prt 0 stype, prt 0 valident1, prt 0 valident2, doc (showString ";")])
    IR.Parser.Gen.AbsIR.IUnOp _ valident unop val -> prPrec i 0 (concatD [prt 0 valident, doc (showString ":="), prt 0 unop, prt 0 val, doc (showString ";")])
    IR.Parser.Gen.AbsIR.IVCall _ call -> prPrec i 0 (concatD [prt 0 call, doc (showString ";")])
    IR.Parser.Gen.AbsIR.ICall _ valident call -> prPrec i 0 (concatD [prt 0 valident, doc (showString ":="), prt 0 call, doc (showString ";")])
    IR.Parser.Gen.AbsIR.INew _ valident stype -> prPrec i 0 (concatD [prt 0 valident, doc (showString ":="), doc (showString "new"), prt 0 stype, doc (showString ";")])
    IR.Parser.Gen.AbsIR.INewArr _ valident stype val -> prPrec i 0 (concatD [prt 0 valident, doc (showString ":="), doc (showString "newarr"), prt 0 stype, doc (showString "["), prt 0 val, doc (showString "]"), doc (showString ";")])
    IR.Parser.Gen.AbsIR.INewStr _ valident str -> prPrec i 0 (concatD [prt 0 valident, doc (showString ":="), doc (showString "newstr"), printString str, doc (showString ";")])
    IR.Parser.Gen.AbsIR.IJmp _ labident -> prPrec i 0 (concatD [doc (showString "jump"), prt 0 labident, doc (showString ";")])
    IR.Parser.Gen.AbsIR.ICondJmp _ val labident1 labident2 -> prPrec i 0 (concatD [doc (showString "jump"), doc (showString "if"), prt 0 val, doc (showString "then"), prt 0 labident1, doc (showString "else"), prt 0 labident2, doc (showString ";")])
    IR.Parser.Gen.AbsIR.ILoad _ valident ptr -> prPrec i 0 (concatD [prt 0 valident, doc (showString ":="), doc (showString "load"), prt 0 ptr, doc (showString ";")])
    IR.Parser.Gen.AbsIR.IStore _ val ptr -> prPrec i 0 (concatD [doc (showString "store"), prt 0 val, doc (showString "into"), prt 0 ptr, doc (showString ";")])
    IR.Parser.Gen.AbsIR.IPhi _ valident phivariants -> prPrec i 0 (concatD [prt 0 valident, doc (showString ":="), doc (showString "phi"), doc (showString "("), prt 0 phivariants, doc (showString ")"), doc (showString ";")])
    IR.Parser.Gen.AbsIR.IEndPhi _ -> prPrec i 0 (concatD [doc (showString "endphi"), doc (showString ";")])

instance Print (IR.Parser.Gen.AbsIR.Ptr' a) where
  prt i = \case
    IR.Parser.Gen.AbsIR.PFld _ stype val qident -> prPrec i 0 (concatD [prt 0 stype, doc (showString "fldptr"), prt 0 val, prt 0 qident])
    IR.Parser.Gen.AbsIR.PElem _ stype val1 val2 -> prPrec i 0 (concatD [prt 0 stype, doc (showString "elemptr"), prt 0 val1, doc (showString "["), prt 0 val2, doc (showString "]")])
    IR.Parser.Gen.AbsIR.PArrLen _ val -> prPrec i 0 (concatD [doc (showString "arrlen"), prt 0 val])
    IR.Parser.Gen.AbsIR.PLocal _ stype n -> prPrec i 0 (concatD [prt 0 stype, doc (showString "local"), prt 0 n])
    IR.Parser.Gen.AbsIR.PParam _ stype n valident -> prPrec i 0 (concatD [prt 0 stype, doc (showString "param"), prt 0 n, prt 0 valident])

instance Print (IR.Parser.Gen.AbsIR.PhiVariant' a) where
  prt i = \case
    IR.Parser.Gen.AbsIR.PhiVar _ labident val -> prPrec i 0 (concatD [prt 0 labident, doc (showString ":"), prt 0 val])

instance Print (IR.Parser.Gen.AbsIR.Call' a) where
  prt i = \case
    IR.Parser.Gen.AbsIR.Call _ stype qident vals -> prPrec i 0 (concatD [doc (showString "call"), prt 0 stype, prt 0 qident, doc (showString "("), prt 0 vals, doc (showString ")")])
    IR.Parser.Gen.AbsIR.CallVirt _ stype qident vals -> prPrec i 0 (concatD [doc (showString "callvirt"), prt 0 stype, prt 0 qident, doc (showString "("), prt 0 vals, doc (showString ")")])

instance Print [IR.Parser.Gen.AbsIR.Instr' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [IR.Parser.Gen.AbsIR.Val' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [IR.Parser.Gen.AbsIR.PhiVariant' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (IR.Parser.Gen.AbsIR.Val' a) where
  prt i = \case
    IR.Parser.Gen.AbsIR.VInt _ n -> prPrec i 0 (concatD [prt 0 n])
    IR.Parser.Gen.AbsIR.VNegInt _ n -> prPrec i 0 (concatD [doc (showString "-"), prt 0 n])
    IR.Parser.Gen.AbsIR.VTrue _ -> prPrec i 0 (concatD [doc (showString "true")])
    IR.Parser.Gen.AbsIR.VFalse _ -> prPrec i 0 (concatD [doc (showString "false")])
    IR.Parser.Gen.AbsIR.VNull _ stype -> prPrec i 0 (concatD [prt 0 stype, doc (showString "null")])
    IR.Parser.Gen.AbsIR.VVal _ stype valident -> prPrec i 0 (concatD [prt 0 stype, prt 0 valident])

instance Print (IR.Parser.Gen.AbsIR.Op' a) where
  prt i = \case
    IR.Parser.Gen.AbsIR.OpAdd _ -> prPrec i 0 (concatD [doc (showString "+")])
    IR.Parser.Gen.AbsIR.OpSub _ -> prPrec i 0 (concatD [doc (showString "-")])
    IR.Parser.Gen.AbsIR.OpMul _ -> prPrec i 0 (concatD [doc (showString "*")])
    IR.Parser.Gen.AbsIR.OpDiv _ -> prPrec i 0 (concatD [doc (showString "/")])
    IR.Parser.Gen.AbsIR.OpMod _ -> prPrec i 0 (concatD [doc (showString "%")])
    IR.Parser.Gen.AbsIR.OpLTH _ -> prPrec i 0 (concatD [doc (showString "<")])
    IR.Parser.Gen.AbsIR.OpLE _ -> prPrec i 0 (concatD [doc (showString "<=")])
    IR.Parser.Gen.AbsIR.OpGTH _ -> prPrec i 0 (concatD [doc (showString ">")])
    IR.Parser.Gen.AbsIR.OpGE _ -> prPrec i 0 (concatD [doc (showString ">=")])
    IR.Parser.Gen.AbsIR.OpEQU _ -> prPrec i 0 (concatD [doc (showString "==")])
    IR.Parser.Gen.AbsIR.OpNE _ -> prPrec i 0 (concatD [doc (showString "!=")])

instance Print (IR.Parser.Gen.AbsIR.UnOp' a) where
  prt i = \case
    IR.Parser.Gen.AbsIR.UnOpNeg _ -> prPrec i 0 (concatD [doc (showString "-")])
    IR.Parser.Gen.AbsIR.UnOpNot _ -> prPrec i 0 (concatD [doc (showString "!")])
