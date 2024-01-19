-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.4).

-- Lexer definition for use with Alex 3
{
{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -w #-}

{-# LANGUAGE PatternSynonyms #-}

module Backend.X64.Parser.Gen.LexXGAS where

import Prelude

import qualified Data.Bits
import Data.Char     (ord)
import Data.Function (on)
import Data.Word     (Word8)
}

-- Predefined character classes

$c = [A-Z\192-\221] # [\215]  -- capital isolatin1 letter (215 = \times) FIXME
$s = [a-z\222-\255] # [\247]  -- small   isolatin1 letter (247 = \div  ) FIXME
$l = [$c $s]         -- letter
$d = [0-9]           -- digit
$i = [$l $d _ ']     -- identifier character
$u = [. \n]          -- universal: any character

-- Symbols and non-identifier-like reserved words

@rsyms = \. "section" | \. "rodata" | \. "text" | \. "global" | \: | \. "string" | \. "quad" | \. "long" | \$ | \. "extern" | \, | \* | \( | \) | \( \% "RIP" \) | \% "RAX" | \% "RBX" | \% "RCX" | \% "RDX" | \% "RDI" | \% "RSI" | \% "RSP" | \% "RBP" | \% "R8" | \% "R9" | \% "R10" | \% "R11" | \% "R12" | \% "R13" | \% "R14" | \% "R15" | \% "EAX" | \% "EBX" | \% "ECX" | \% "EDX" | \% "EDI" | \% "ESI" | \% "ESP" | \% "EBP" | \% "R8D" | \% "R9D" | \% "R10D" | \% "R11D" | \% "R12D" | \% "R13D" | \% "R14D" | \% "R15D" | \% "AX" | \% "BX" | \% "CX" | \% "DX" | \% "DI" | \% "SI" | \% "SP" | \% "BP" | \% "R8W" | \% "R9W" | \% "R10W" | \% "R11W" | \% "R12W" | \% "R13W" | \% "R14W" | \% "R15W" | \% "AL" | \% "BL" | \% "CL" | \% "DL" | \% "DIL" | \% "SIL" | \% "SPL" | \% "BPL" | \% "R8B" | \% "R9B" | \% "R10B" | \% "R11B" | \% "R12B" | \% "R13B" | \% "R14B" | \% "R15B"

:-

-- Line comment "#"
"#" [.]* ;

-- Whitespace (skipped)
$white+ ;

-- Symbols
@rsyms
    { tok (eitherResIdent TV) }

-- token Label
([\' \_]| ($d | $l)) +
    { tok (eitherResIdent T_Label) }

-- Keywords and Ident
$l $i*
    { tok (eitherResIdent TV) }

-- String
\" ([$u # [\" \\ \n]] | (\\ (\" | \\ | \' | n | t | r | f)))* \"
    { tok (TL . unescapeInitTail) }

-- Integer
$d+
    { tok TI }

{
-- | Create a token with position.
tok :: (String -> Tok) -> (Posn -> String -> Token)
tok f p = PT p . f

-- | Token without position.
data Tok
  = TK {-# UNPACK #-} !TokSymbol  -- ^ Reserved word or symbol.
  | TL !String                    -- ^ String literal.
  | TI !String                    -- ^ Integer literal.
  | TV !String                    -- ^ Identifier.
  | TD !String                    -- ^ Float literal.
  | TC !String                    -- ^ Character literal.
  | T_Label !String
  deriving (Eq, Show, Ord)

-- | Smart constructor for 'Tok' for the sake of backwards compatibility.
pattern TS :: String -> Int -> Tok
pattern TS t i = TK (TokSymbol t i)

-- | Keyword or symbol tokens have a unique ID.
data TokSymbol = TokSymbol
  { tsText :: String
      -- ^ Keyword or symbol text.
  , tsID   :: !Int
      -- ^ Unique ID.
  } deriving (Show)

-- | Keyword/symbol equality is determined by the unique ID.
instance Eq  TokSymbol where (==)    = (==)    `on` tsID

-- | Keyword/symbol ordering is determined by the unique ID.
instance Ord TokSymbol where compare = compare `on` tsID

-- | Token with position.
data Token
  = PT  Posn Tok
  | Err Posn
  deriving (Eq, Show, Ord)

-- | Pretty print a position.
printPosn :: Posn -> String
printPosn (Pn _ l c) = "line " ++ show l ++ ", column " ++ show c

-- | Pretty print the position of the first token in the list.
tokenPos :: [Token] -> String
tokenPos (t:_) = printPosn (tokenPosn t)
tokenPos []    = "end of file"

-- | Get the position of a token.
tokenPosn :: Token -> Posn
tokenPosn (PT p _) = p
tokenPosn (Err p)  = p

-- | Get line and column of a token.
tokenLineCol :: Token -> (Int, Int)
tokenLineCol = posLineCol . tokenPosn

-- | Get line and column of a position.
posLineCol :: Posn -> (Int, Int)
posLineCol (Pn _ l c) = (l,c)

-- | Convert a token into "position token" form.
mkPosToken :: Token -> ((Int, Int), String)
mkPosToken t = (tokenLineCol t, tokenText t)

-- | Convert a token to its text.
tokenText :: Token -> String
tokenText t = case t of
  PT _ (TS s _) -> s
  PT _ (TL s)   -> show s
  PT _ (TI s)   -> s
  PT _ (TV s)   -> s
  PT _ (TD s)   -> s
  PT _ (TC s)   -> s
  Err _         -> "#error"
  PT _ (T_Label s) -> s

-- | Convert a token to a string.
prToken :: Token -> String
prToken t = tokenText t

-- | Finite map from text to token organized as binary search tree.
data BTree
  = N -- ^ Nil (leaf).
  | B String Tok BTree BTree
      -- ^ Binary node.
  deriving (Show)

-- | Convert potential keyword into token or use fallback conversion.
eitherResIdent :: (String -> Tok) -> String -> Tok
eitherResIdent tv s = treeFind resWords
  where
  treeFind N = tv s
  treeFind (B a t left right) =
    case compare s a of
      LT -> treeFind left
      GT -> treeFind right
      EQ -> t

-- | The keywords and symbols of the language organized as binary search tree.
resWords :: BTree
resWords =
  b ")" 68
    (b "%R13" 34
       (b "%ECX" 17
          (b "%CX" 9
             (b "%BP" 5
                (b "%AX" 3 (b "%AL" 2 (b "$" 1 N N) N) (b "%BL" 4 N N))
                (b "%BX" 7 (b "%BPL" 6 N N) (b "%CL" 8 N N)))
             (b "%DX" 13
                (b "%DIL" 11 (b "%DI" 10 N N) (b "%DL" 12 N N))
                (b "%EBP" 15 (b "%EAX" 14 N N) (b "%EBX" 16 N N))))
          (b "%R11" 26
             (b "%R10" 22
                (b "%ESI" 20 (b "%EDX" 19 (b "%EDI" 18 N N) N) (b "%ESP" 21 N N))
                (b "%R10D" 24 (b "%R10B" 23 N N) (b "%R10W" 25 N N)))
             (b "%R12" 30
                (b "%R11D" 28 (b "%R11B" 27 N N) (b "%R11W" 29 N N))
                (b "%R12D" 32 (b "%R12B" 31 N N) (b "%R12W" 33 N N)))))
       (b "%R9B" 51
          (b "%R15B" 43
             (b "%R14B" 39
                (b "%R13W" 37
                   (b "%R13D" 36 (b "%R13B" 35 N N) N) (b "%R14" 38 N N))
                (b "%R14W" 41 (b "%R14D" 40 N N) (b "%R15" 42 N N)))
             (b "%R8B" 47
                (b "%R15W" 45 (b "%R15D" 44 N N) (b "%R8" 46 N N))
                (b "%R8W" 49 (b "%R8D" 48 N N) (b "%R9" 50 N N))))
          (b "%RSI" 60
             (b "%RBX" 56
                (b "%RAX" 54 (b "%R9W" 53 (b "%R9D" 52 N N) N) (b "%RBP" 55 N N))
                (b "%RDI" 58 (b "%RCX" 57 N N) (b "%RDX" 59 N N)))
             (b "%SP" 64
                (b "%SI" 62 (b "%RSP" 61 N N) (b "%SIL" 63 N N))
                (b "(" 66 (b "%SPL" 65 N N) (b "(%RIP)" 67 N N))))))
    (b "leave" 102
       (b "andl" 85
          (b ".string" 77
             (b ".long" 73
                (b ".extern" 71 (b "," 70 (b "*" 69 N N) N) (b ".global" 72 N N))
                (b ".rodata" 75 (b ".quad" 74 N N) (b ".section" 76 N N)))
             (b "addb" 81
                (b ":" 79 (b ".text" 78 N N) (b "CALL" 80 N N))
                (b "addq" 83 (b "addl" 82 N N) (b "andb" 84 N N))))
          (b "imulb" 94
             (b "cmpq" 90
                (b "cmpb" 88 (b "cdq" 87 (b "andq" 86 N N) N) (b "cmpl" 89 N N))
                (b "idivl" 92 (b "idivb" 91 N N) (b "idivq" 93 N N)))
             (b "jz" 98
                (b "imulq" 96 (b "imull" 95 N N) (b "jmp" 97 N N))
                (b "leal" 100 (b "leab" 99 N N) (b "leaq" 101 N N)))))
       (b "setg" 119
          (b "ret" 111
             (b "negl" 107
                (b "movq" 105
                   (b "movl" 104 (b "movb" 103 N N) N) (b "negb" 106 N N))
                (b "pop" 109 (b "negq" 108 N N) (b "push" 110 N N)))
             (b "sarb" 115
                (b "sall" 113 (b "salb" 112 N N) (b "salq" 114 N N))
                (b "sarq" 117 (b "sarl" 116 N N) (b "sete" 118 N N))))
          (b "testl" 128
             (b "subb" 124
                (b "setle" 122
                   (b "setl" 121 (b "setge" 120 N N) N) (b "setne" 123 N N))
                (b "subq" 126 (b "subl" 125 N N) (b "testb" 127 N N)))
             (b "xchgq" 132
                (b "xchgb" 130 (b "testq" 129 N N) (b "xchgl" 131 N N))
                (b "xorl" 134 (b "xorb" 133 N N) (b "xorq" 135 N N))))))
  where
  b s n = B bs (TS bs n)
    where
    bs = s

-- | Unquote string literal.
unescapeInitTail :: String -> String
unescapeInitTail = id . unesc . tail . id
  where
  unesc s = case s of
    '\\':c:cs | elem c ['\"', '\\', '\''] -> c : unesc cs
    '\\':'n':cs  -> '\n' : unesc cs
    '\\':'t':cs  -> '\t' : unesc cs
    '\\':'r':cs  -> '\r' : unesc cs
    '\\':'f':cs  -> '\f' : unesc cs
    '"':[]       -> []
    c:cs         -> c : unesc cs
    _            -> []

-------------------------------------------------------------------
-- Alex wrapper code.
-- A modified "posn" wrapper.
-------------------------------------------------------------------

data Posn = Pn !Int !Int !Int
  deriving (Eq, Show, Ord)

alexStartPos :: Posn
alexStartPos = Pn 0 1 1

alexMove :: Posn -> Char -> Posn
alexMove (Pn a l c) '\t' = Pn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (Pn a l c) '\n' = Pn (a+1) (l+1)   1
alexMove (Pn a l c) _    = Pn (a+1)  l     (c+1)

type Byte = Word8

type AlexInput = (Posn,     -- current position,
                  Char,     -- previous char
                  [Byte],   -- pending bytes on the current char
                  String)   -- current input string

tokens :: String -> [Token]
tokens str = go (alexStartPos, '\n', [], str)
    where
      go :: AlexInput -> [Token]
      go inp@(pos, _, _, str) =
               case alexScan inp 0 of
                AlexEOF                   -> []
                AlexError (pos, _, _, _)  -> [Err pos]
                AlexSkip  inp' len        -> go inp'
                AlexToken inp' len act    -> act pos (take len str) : (go inp')

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p, c, (b:bs), s) = Just (b, (p, c, bs, s))
alexGetByte (p, _, [], s) =
  case s of
    []  -> Nothing
    (c:s) ->
             let p'     = alexMove p c
                 (b:bs) = utf8Encode c
              in p' `seq` Just (b, (p', c, bs, s))

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p, c, bs, s) = c

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
  where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
}
