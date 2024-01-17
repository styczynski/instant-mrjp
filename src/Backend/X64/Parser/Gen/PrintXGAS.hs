-- File generated by the BNF Converter (bnfc 2.9.4).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for Backend.

module Backend.X64.Parser.Gen.PrintXGAS where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified Backend.X64.Parser.Gen.AbsXGAS

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

instance Print Backend.X64.Parser.Gen.AbsXGAS.Label where
  prt _ (Backend.X64.Parser.Gen.AbsXGAS.Label i) = doc $ showString i
instance Print (Backend.X64.Parser.Gen.AbsXGAS.AsmProgram' a) where
  prt i = \case
    Backend.X64.Parser.Gen.AbsXGAS.AsmProgram _ directives sectiondata sectioncode -> prPrec i 0 (concatD [prt 0 directives, prt 0 sectiondata, prt 0 sectioncode])

instance Print [Backend.X64.Parser.Gen.AbsXGAS.AsmInstr' a] where
  prt _ [] = concatD []
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString "\n"), prt 0 xs]

instance Print (Backend.X64.Parser.Gen.AbsXGAS.SectionData' a) where
  prt i = \case
    Backend.X64.Parser.Gen.AbsXGAS.SectionData _ asmdatadefs -> prPrec i 0 (concatD [doc (showString ".section"), doc (showString ".rodata"), prt 0 asmdatadefs])

instance Print (Backend.X64.Parser.Gen.AbsXGAS.SectionCode' a) where
  prt i = \case
    Backend.X64.Parser.Gen.AbsXGAS.SectionCode _ asminstrs -> prPrec i 0 (concatD [doc (showString ".section"), doc (showString ".text"), prt 0 asminstrs])

instance Print (Backend.X64.Parser.Gen.AbsXGAS.AsmDataDef' a) where
  prt i = \case
    Backend.X64.Parser.Gen.AbsXGAS.AsmDataGlobal _ label -> prPrec i 0 (concatD [doc (showString ".global"), prt 0 label])
    Backend.X64.Parser.Gen.AbsXGAS.AsmDataDef _ label datas -> prPrec i 0 (concatD [prt 0 label, doc (showString ":"), prt 0 datas])

instance Print [Backend.X64.Parser.Gen.AbsXGAS.AsmDataDef' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, doc (showString "\n"), prt 0 xs]

instance Print (Backend.X64.Parser.Gen.AbsXGAS.Data' a) where
  prt i = \case
    Backend.X64.Parser.Gen.AbsXGAS.DataString _ str -> prPrec i 0 (concatD [doc (showString ".string"), printString str])
    Backend.X64.Parser.Gen.AbsXGAS.Data64 _ dataconst -> prPrec i 0 (concatD [doc (showString ".quad"), prt 0 dataconst])
    Backend.X64.Parser.Gen.AbsXGAS.Data32 _ dataconst -> prPrec i 0 (concatD [doc (showString ".long"), prt 0 dataconst])

instance Print [Backend.X64.Parser.Gen.AbsXGAS.Data' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, doc (showString "\n"), prt 0 xs]

instance Print (Backend.X64.Parser.Gen.AbsXGAS.DataConst' a) where
  prt i = \case
    Backend.X64.Parser.Gen.AbsXGAS.ConstInt _ n -> prPrec i 0 (concatD [doc (showString "$"), prt 0 n])
    Backend.X64.Parser.Gen.AbsXGAS.ConstLabel _ label -> prPrec i 0 (concatD [prt 0 label])

instance Print (Backend.X64.Parser.Gen.AbsXGAS.Directive' a) where
  prt i = \case
    Backend.X64.Parser.Gen.AbsXGAS.Extern _ label -> prPrec i 0 (concatD [doc (showString ".extern"), prt 0 label])

instance Print [Backend.X64.Parser.Gen.AbsXGAS.Directive' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, doc (showString "\n"), prt 0 xs]

instance Print (Backend.X64.Parser.Gen.AbsXGAS.AsmInstr' a) where
  prt i = \case
    Backend.X64.Parser.Gen.AbsXGAS.LabelDef _ label -> prPrec i 0 (concatD [prt 0 label, doc (showString ":")])
    Backend.X64.Parser.Gen.AbsXGAS.ADD64 _ source target -> prPrec i 0 (concatD [doc (showString "addq"), prt 64 source, doc (showString ","), prt 64 target])
    Backend.X64.Parser.Gen.AbsXGAS.AND64 _ source target -> prPrec i 0 (concatD [doc (showString "andq"), prt 64 source, doc (showString ","), prt 64 target])
    Backend.X64.Parser.Gen.AbsXGAS.CMP64 _ source target -> prPrec i 0 (concatD [doc (showString "cmpq"), prt 64 source, doc (showString ","), prt 64 target])
    Backend.X64.Parser.Gen.AbsXGAS.IDIV64 _ source target -> prPrec i 0 (concatD [doc (showString "idivq"), prt 64 source, doc (showString ","), prt 64 target])
    Backend.X64.Parser.Gen.AbsXGAS.IMUL64 _ source target -> prPrec i 0 (concatD [doc (showString "imulq"), prt 64 source, doc (showString ","), prt 64 target])
    Backend.X64.Parser.Gen.AbsXGAS.LEA64 _ source target -> prPrec i 0 (concatD [doc (showString "leaq"), prt 64 source, doc (showString ","), prt 64 target])
    Backend.X64.Parser.Gen.AbsXGAS.MOV64 _ source target -> prPrec i 0 (concatD [doc (showString "movq"), prt 64 source, doc (showString ","), prt 64 target])
    Backend.X64.Parser.Gen.AbsXGAS.SUB64 _ source target -> prPrec i 0 (concatD [doc (showString "subq"), prt 64 source, doc (showString ","), prt 64 target])
    Backend.X64.Parser.Gen.AbsXGAS.TEST64 _ source target -> prPrec i 0 (concatD [doc (showString "testq"), prt 64 source, doc (showString ","), prt 64 target])
    Backend.X64.Parser.Gen.AbsXGAS.XOR64 _ source target -> prPrec i 0 (concatD [doc (showString "xorq"), prt 64 source, doc (showString ","), prt 64 target])
    Backend.X64.Parser.Gen.AbsXGAS.XCHG64 _ source target -> prPrec i 0 (concatD [doc (showString "xchgq"), prt 64 source, doc (showString ","), prt 64 target])
    Backend.X64.Parser.Gen.AbsXGAS.SAL64 _ source target -> prPrec i 0 (concatD [doc (showString "salq"), prt 64 source, doc (showString ","), prt 64 target])
    Backend.X64.Parser.Gen.AbsXGAS.SAR64 _ source target -> prPrec i 0 (concatD [doc (showString "sarq"), prt 64 source, doc (showString ","), prt 64 target])
    Backend.X64.Parser.Gen.AbsXGAS.ADD32 _ source target -> prPrec i 0 (concatD [doc (showString "addl"), prt 32 source, doc (showString ","), prt 32 target])
    Backend.X64.Parser.Gen.AbsXGAS.AND32 _ source target -> prPrec i 0 (concatD [doc (showString "andl"), prt 32 source, doc (showString ","), prt 32 target])
    Backend.X64.Parser.Gen.AbsXGAS.CMP32 _ source target -> prPrec i 0 (concatD [doc (showString "cmpl"), prt 32 source, doc (showString ","), prt 32 target])
    Backend.X64.Parser.Gen.AbsXGAS.IDIV32 _ source target -> prPrec i 0 (concatD [doc (showString "idivl"), prt 32 source, doc (showString ","), prt 32 target])
    Backend.X64.Parser.Gen.AbsXGAS.IMUL32 _ source target -> prPrec i 0 (concatD [doc (showString "imull"), prt 32 source, doc (showString ","), prt 32 target])
    Backend.X64.Parser.Gen.AbsXGAS.LEA32 _ source target -> prPrec i 0 (concatD [doc (showString "leal"), prt 32 source, doc (showString ","), prt 32 target])
    Backend.X64.Parser.Gen.AbsXGAS.MOV32 _ source target -> prPrec i 0 (concatD [doc (showString "movl"), prt 32 source, doc (showString ","), prt 32 target])
    Backend.X64.Parser.Gen.AbsXGAS.SUB32 _ source target -> prPrec i 0 (concatD [doc (showString "subl"), prt 32 source, doc (showString ","), prt 32 target])
    Backend.X64.Parser.Gen.AbsXGAS.TEST32 _ source target -> prPrec i 0 (concatD [doc (showString "testl"), prt 32 source, doc (showString ","), prt 32 target])
    Backend.X64.Parser.Gen.AbsXGAS.XOR32 _ source target -> prPrec i 0 (concatD [doc (showString "xorl"), prt 32 source, doc (showString ","), prt 32 target])
    Backend.X64.Parser.Gen.AbsXGAS.XCHG32 _ source target -> prPrec i 0 (concatD [doc (showString "xchgl"), prt 32 source, doc (showString ","), prt 32 target])
    Backend.X64.Parser.Gen.AbsXGAS.SAL32 _ source target -> prPrec i 0 (concatD [doc (showString "sall"), prt 32 source, doc (showString ","), prt 32 target])
    Backend.X64.Parser.Gen.AbsXGAS.SAR32 _ source target -> prPrec i 0 (concatD [doc (showString "sarl"), prt 32 source, doc (showString ","), prt 32 target])
    Backend.X64.Parser.Gen.AbsXGAS.ADD16 _ source target -> prPrec i 0 (concatD [doc (showString "addb"), prt 16 source, doc (showString ","), prt 16 target])
    Backend.X64.Parser.Gen.AbsXGAS.AND16 _ source target -> prPrec i 0 (concatD [doc (showString "andb"), prt 16 source, doc (showString ","), prt 16 target])
    Backend.X64.Parser.Gen.AbsXGAS.CMP16 _ source target -> prPrec i 0 (concatD [doc (showString "cmpb"), prt 16 source, doc (showString ","), prt 16 target])
    Backend.X64.Parser.Gen.AbsXGAS.IDIV16 _ source target -> prPrec i 0 (concatD [doc (showString "idivb"), prt 16 source, doc (showString ","), prt 16 target])
    Backend.X64.Parser.Gen.AbsXGAS.IMUL16 _ source target -> prPrec i 0 (concatD [doc (showString "imulb"), prt 16 source, doc (showString ","), prt 16 target])
    Backend.X64.Parser.Gen.AbsXGAS.LEA16 _ source target -> prPrec i 0 (concatD [doc (showString "leab"), prt 16 source, doc (showString ","), prt 16 target])
    Backend.X64.Parser.Gen.AbsXGAS.MOV16 _ source target -> prPrec i 0 (concatD [doc (showString "movb"), prt 16 source, doc (showString ","), prt 16 target])
    Backend.X64.Parser.Gen.AbsXGAS.SUB16 _ source target -> prPrec i 0 (concatD [doc (showString "subb"), prt 16 source, doc (showString ","), prt 16 target])
    Backend.X64.Parser.Gen.AbsXGAS.TEST16 _ source target -> prPrec i 0 (concatD [doc (showString "testb"), prt 16 source, doc (showString ","), prt 16 target])
    Backend.X64.Parser.Gen.AbsXGAS.XOR16 _ source target -> prPrec i 0 (concatD [doc (showString "xorb"), prt 16 source, doc (showString ","), prt 16 target])
    Backend.X64.Parser.Gen.AbsXGAS.XCHG16 _ source target -> prPrec i 0 (concatD [doc (showString "xchgb"), prt 16 source, doc (showString ","), prt 16 target])
    Backend.X64.Parser.Gen.AbsXGAS.SAL16 _ source target -> prPrec i 0 (concatD [doc (showString "salb"), prt 16 source, doc (showString ","), prt 16 target])
    Backend.X64.Parser.Gen.AbsXGAS.SAR16 _ source target -> prPrec i 0 (concatD [doc (showString "sarb"), prt 16 source, doc (showString ","), prt 16 target])
    Backend.X64.Parser.Gen.AbsXGAS.NEG64 _ target -> prPrec i 0 (concatD [doc (showString "negq"), prt 64 target])
    Backend.X64.Parser.Gen.AbsXGAS.NEG32 _ target -> prPrec i 0 (concatD [doc (showString "negl"), prt 32 target])
    Backend.X64.Parser.Gen.AbsXGAS.NEG16 _ target -> prPrec i 0 (concatD [doc (showString "negb"), prt 16 target])
    Backend.X64.Parser.Gen.AbsXGAS.POP _ reg -> prPrec i 0 (concatD [doc (showString "pop"), prt 64 reg])
    Backend.X64.Parser.Gen.AbsXGAS.PUSH _ reg -> prPrec i 0 (concatD [doc (showString "push"), prt 64 reg])
    Backend.X64.Parser.Gen.AbsXGAS.LEAVE _ -> prPrec i 0 (concatD [doc (showString "leave")])
    Backend.X64.Parser.Gen.AbsXGAS.RET _ -> prPrec i 0 (concatD [doc (showString "ret")])
    Backend.X64.Parser.Gen.AbsXGAS.CDQ _ -> prPrec i 0 (concatD [doc (showString "cdq")])
    Backend.X64.Parser.Gen.AbsXGAS.SETE _ reg -> prPrec i 0 (concatD [doc (showString "sete"), prt 8 reg])
    Backend.X64.Parser.Gen.AbsXGAS.SETG _ reg -> prPrec i 0 (concatD [doc (showString "setg"), prt 8 reg])
    Backend.X64.Parser.Gen.AbsXGAS.SETGE _ reg -> prPrec i 0 (concatD [doc (showString "setge"), prt 8 reg])
    Backend.X64.Parser.Gen.AbsXGAS.SETL _ reg -> prPrec i 0 (concatD [doc (showString "setl"), prt 8 reg])
    Backend.X64.Parser.Gen.AbsXGAS.SETLE _ reg -> prPrec i 0 (concatD [doc (showString "setle"), prt 8 reg])
    Backend.X64.Parser.Gen.AbsXGAS.SETNE _ reg -> prPrec i 0 (concatD [doc (showString "setne"), prt 8 reg])
    Backend.X64.Parser.Gen.AbsXGAS.JMP _ label -> prPrec i 0 (concatD [doc (showString "jmp"), prt 0 label])
    Backend.X64.Parser.Gen.AbsXGAS.JZ _ label -> prPrec i 0 (concatD [doc (showString "jz"), prt 0 label])

instance Print (Backend.X64.Parser.Gen.AbsXGAS.Target' a) where
  prt i = \case
    Backend.X64.Parser.Gen.AbsXGAS.ToReg64 _ reg -> prPrec i 64 (concatD [prt 64 reg])
    Backend.X64.Parser.Gen.AbsXGAS.ToMem64 _ n reg -> prPrec i 64 (concatD [prt 0 n, doc (showString "("), prt 64 reg, doc (showString ")")])
    Backend.X64.Parser.Gen.AbsXGAS.ToReg32 _ reg -> prPrec i 32 (concatD [prt 32 reg])
    Backend.X64.Parser.Gen.AbsXGAS.ToMem32 _ n reg -> prPrec i 32 (concatD [prt 0 n, doc (showString "("), prt 32 reg, doc (showString ")")])
    Backend.X64.Parser.Gen.AbsXGAS.ToReg16 _ reg -> prPrec i 16 (concatD [prt 16 reg])
    Backend.X64.Parser.Gen.AbsXGAS.ToMem16 _ n reg -> prPrec i 16 (concatD [prt 0 n, doc (showString "("), prt 16 reg, doc (showString ")")])
    Backend.X64.Parser.Gen.AbsXGAS.ToReg8 _ reg -> prPrec i 8 (concatD [prt 8 reg])
    Backend.X64.Parser.Gen.AbsXGAS.ToMem8 _ n reg -> prPrec i 8 (concatD [prt 0 n, doc (showString "("), prt 8 reg, doc (showString ")")])

instance Print (Backend.X64.Parser.Gen.AbsXGAS.Source' a) where
  prt i = \case
    Backend.X64.Parser.Gen.AbsXGAS.FromReg64 _ reg -> prPrec i 64 (concatD [prt 64 reg])
    Backend.X64.Parser.Gen.AbsXGAS.FromMem64 _ n reg -> prPrec i 64 (concatD [prt 0 n, doc (showString "("), prt 64 reg, doc (showString ")")])
    Backend.X64.Parser.Gen.AbsXGAS.FromReg32 _ reg -> prPrec i 32 (concatD [prt 32 reg])
    Backend.X64.Parser.Gen.AbsXGAS.FromMem32 _ n reg -> prPrec i 32 (concatD [prt 0 n, doc (showString "("), prt 32 reg, doc (showString ")")])
    Backend.X64.Parser.Gen.AbsXGAS.FromReg16 _ reg -> prPrec i 16 (concatD [prt 16 reg])
    Backend.X64.Parser.Gen.AbsXGAS.FromMem16 _ n reg -> prPrec i 16 (concatD [prt 0 n, doc (showString "("), prt 16 reg, doc (showString ")")])
    Backend.X64.Parser.Gen.AbsXGAS.FromReg8 _ reg -> prPrec i 8 (concatD [prt 8 reg])
    Backend.X64.Parser.Gen.AbsXGAS.FromMem8 _ n reg -> prPrec i 8 (concatD [prt 0 n, doc (showString "("), prt 8 reg, doc (showString ")")])

instance Print (Backend.X64.Parser.Gen.AbsXGAS.Reg' a) where
  prt i = \case
    Backend.X64.Parser.Gen.AbsXGAS.RAX _ -> prPrec i 64 (concatD [doc (showString "%RAX")])
    Backend.X64.Parser.Gen.AbsXGAS.RBX _ -> prPrec i 64 (concatD [doc (showString "%RBX")])
    Backend.X64.Parser.Gen.AbsXGAS.RCX _ -> prPrec i 64 (concatD [doc (showString "%RCX")])
    Backend.X64.Parser.Gen.AbsXGAS.RDX _ -> prPrec i 64 (concatD [doc (showString "%RDX")])
    Backend.X64.Parser.Gen.AbsXGAS.RDI _ -> prPrec i 64 (concatD [doc (showString "%RDI")])
    Backend.X64.Parser.Gen.AbsXGAS.RSI _ -> prPrec i 64 (concatD [doc (showString "%RSI")])
    Backend.X64.Parser.Gen.AbsXGAS.RSP _ -> prPrec i 64 (concatD [doc (showString "%RSP")])
    Backend.X64.Parser.Gen.AbsXGAS.RBP _ -> prPrec i 64 (concatD [doc (showString "%RBP")])
    Backend.X64.Parser.Gen.AbsXGAS.R8 _ -> prPrec i 64 (concatD [doc (showString "%R8")])
    Backend.X64.Parser.Gen.AbsXGAS.R9 _ -> prPrec i 64 (concatD [doc (showString "%R9")])
    Backend.X64.Parser.Gen.AbsXGAS.R10 _ -> prPrec i 64 (concatD [doc (showString "%R10")])
    Backend.X64.Parser.Gen.AbsXGAS.R11 _ -> prPrec i 64 (concatD [doc (showString "%R11")])
    Backend.X64.Parser.Gen.AbsXGAS.R12 _ -> prPrec i 64 (concatD [doc (showString "%R12")])
    Backend.X64.Parser.Gen.AbsXGAS.R13 _ -> prPrec i 64 (concatD [doc (showString "%R13")])
    Backend.X64.Parser.Gen.AbsXGAS.R14 _ -> prPrec i 64 (concatD [doc (showString "%R14")])
    Backend.X64.Parser.Gen.AbsXGAS.R15 _ -> prPrec i 64 (concatD [doc (showString "%R15")])
    Backend.X64.Parser.Gen.AbsXGAS.EAX _ -> prPrec i 32 (concatD [doc (showString "%EAX")])
    Backend.X64.Parser.Gen.AbsXGAS.EBX _ -> prPrec i 32 (concatD [doc (showString "%EBX")])
    Backend.X64.Parser.Gen.AbsXGAS.ECX _ -> prPrec i 32 (concatD [doc (showString "%ECX")])
    Backend.X64.Parser.Gen.AbsXGAS.EDX _ -> prPrec i 32 (concatD [doc (showString "%EDX")])
    Backend.X64.Parser.Gen.AbsXGAS.EDI _ -> prPrec i 32 (concatD [doc (showString "%EDI")])
    Backend.X64.Parser.Gen.AbsXGAS.ESI _ -> prPrec i 32 (concatD [doc (showString "%ESI")])
    Backend.X64.Parser.Gen.AbsXGAS.ESP _ -> prPrec i 32 (concatD [doc (showString "%ESP")])
    Backend.X64.Parser.Gen.AbsXGAS.EBP _ -> prPrec i 32 (concatD [doc (showString "%EBP")])
    Backend.X64.Parser.Gen.AbsXGAS.R8D _ -> prPrec i 32 (concatD [doc (showString "%R8D")])
    Backend.X64.Parser.Gen.AbsXGAS.R9D _ -> prPrec i 32 (concatD [doc (showString "%R9D")])
    Backend.X64.Parser.Gen.AbsXGAS.R10D _ -> prPrec i 32 (concatD [doc (showString "%R10D")])
    Backend.X64.Parser.Gen.AbsXGAS.R11D _ -> prPrec i 32 (concatD [doc (showString "%R11D")])
    Backend.X64.Parser.Gen.AbsXGAS.R12D _ -> prPrec i 32 (concatD [doc (showString "%R12D")])
    Backend.X64.Parser.Gen.AbsXGAS.R13D _ -> prPrec i 32 (concatD [doc (showString "%R13D")])
    Backend.X64.Parser.Gen.AbsXGAS.R14D _ -> prPrec i 32 (concatD [doc (showString "%R14D")])
    Backend.X64.Parser.Gen.AbsXGAS.R15D _ -> prPrec i 32 (concatD [doc (showString "%R15D")])
    Backend.X64.Parser.Gen.AbsXGAS.AX _ -> prPrec i 16 (concatD [doc (showString "%AX")])
    Backend.X64.Parser.Gen.AbsXGAS.BX _ -> prPrec i 16 (concatD [doc (showString "%BX")])
    Backend.X64.Parser.Gen.AbsXGAS.CX _ -> prPrec i 16 (concatD [doc (showString "%CX")])
    Backend.X64.Parser.Gen.AbsXGAS.DX _ -> prPrec i 16 (concatD [doc (showString "%DX")])
    Backend.X64.Parser.Gen.AbsXGAS.DI _ -> prPrec i 16 (concatD [doc (showString "%DI")])
    Backend.X64.Parser.Gen.AbsXGAS.SI _ -> prPrec i 16 (concatD [doc (showString "%SI")])
    Backend.X64.Parser.Gen.AbsXGAS.SP _ -> prPrec i 16 (concatD [doc (showString "%SP")])
    Backend.X64.Parser.Gen.AbsXGAS.BP _ -> prPrec i 16 (concatD [doc (showString "%BP")])
    Backend.X64.Parser.Gen.AbsXGAS.R8W _ -> prPrec i 16 (concatD [doc (showString "%R8W")])
    Backend.X64.Parser.Gen.AbsXGAS.R9W _ -> prPrec i 16 (concatD [doc (showString "%R9W")])
    Backend.X64.Parser.Gen.AbsXGAS.R10W _ -> prPrec i 16 (concatD [doc (showString "%R10W")])
    Backend.X64.Parser.Gen.AbsXGAS.R11W _ -> prPrec i 16 (concatD [doc (showString "%R11W")])
    Backend.X64.Parser.Gen.AbsXGAS.R12W _ -> prPrec i 16 (concatD [doc (showString "%R12W")])
    Backend.X64.Parser.Gen.AbsXGAS.R13W _ -> prPrec i 16 (concatD [doc (showString "%R13W")])
    Backend.X64.Parser.Gen.AbsXGAS.R14W _ -> prPrec i 16 (concatD [doc (showString "%R14W")])
    Backend.X64.Parser.Gen.AbsXGAS.R15W _ -> prPrec i 16 (concatD [doc (showString "%R15W")])
    Backend.X64.Parser.Gen.AbsXGAS.AL _ -> prPrec i 8 (concatD [doc (showString "%AL")])
    Backend.X64.Parser.Gen.AbsXGAS.BL _ -> prPrec i 8 (concatD [doc (showString "%BL")])
    Backend.X64.Parser.Gen.AbsXGAS.CL _ -> prPrec i 8 (concatD [doc (showString "%CL")])
    Backend.X64.Parser.Gen.AbsXGAS.DL _ -> prPrec i 8 (concatD [doc (showString "%DL")])
    Backend.X64.Parser.Gen.AbsXGAS.DIL _ -> prPrec i 8 (concatD [doc (showString "%DIL")])
    Backend.X64.Parser.Gen.AbsXGAS.SIL _ -> prPrec i 8 (concatD [doc (showString "%SIL")])
    Backend.X64.Parser.Gen.AbsXGAS.SPL _ -> prPrec i 8 (concatD [doc (showString "%SPL")])
    Backend.X64.Parser.Gen.AbsXGAS.BPL _ -> prPrec i 8 (concatD [doc (showString "%BPL")])
    Backend.X64.Parser.Gen.AbsXGAS.R8B _ -> prPrec i 8 (concatD [doc (showString "%R8B")])
    Backend.X64.Parser.Gen.AbsXGAS.R9B _ -> prPrec i 8 (concatD [doc (showString "%R9B")])
    Backend.X64.Parser.Gen.AbsXGAS.R10B _ -> prPrec i 8 (concatD [doc (showString "%R10B")])
    Backend.X64.Parser.Gen.AbsXGAS.R11B _ -> prPrec i 8 (concatD [doc (showString "%R11B")])
    Backend.X64.Parser.Gen.AbsXGAS.R12B _ -> prPrec i 8 (concatD [doc (showString "%R12B")])
    Backend.X64.Parser.Gen.AbsXGAS.R13B _ -> prPrec i 8 (concatD [doc (showString "%R13B")])
    Backend.X64.Parser.Gen.AbsXGAS.R14B _ -> prPrec i 8 (concatD [doc (showString "%R14B")])
    Backend.X64.Parser.Gen.AbsXGAS.R15B _ -> prPrec i 8 (concatD [doc (showString "%R15B")])
