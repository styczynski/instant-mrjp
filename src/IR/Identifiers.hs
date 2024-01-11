-- Reserved identifiers used internally by the compiler.
-- Any identifier starting with '~' is meant to be invisible
-- by user code and unspeakable using lexical rules of the language.
module IR.Identifiers where

import           IR.Syntax.Syntax


argValIdent :: String -> ValIdent
argValIdent s = ValIdent $ "%a_" ++ s

constIdent :: String -> String
constIdent = ("__const_" ++)


exitLabel :: LabIdent
exitLabel = LabIdent ".L_exit"


indexedValIdent :: String -> Integer -> ValIdent
indexedValIdent i idx =
    let suf = if idx == 0 then "" else '_':show idx
    in valIdent (i ++ suf)

labIdent :: String -> LabIdent
labIdent = LabIdent . (".L_" ++)

phiUnfoldJumpFromToLabel :: LabIdent -> LabIdent -> LabIdent
phiUnfoldJumpFromToLabel (LabIdent from) (LabIdent to) = LabIdent $ to ++ "__from_" ++ trim from
    where
        trim ('.':'L':'_':xs) = xs
        trim xs               = xs



getCallTarget :: QIdent a -> String
getCallTarget (QIdent _ (SymIdent i1) (SymIdent i2)) =
    if i1 == "~cl_TopLevel"
    then case i2 of
        "readInt"     -> "lat_read_int"
        "readString"  -> "lat_read_string"
        "printInt"    -> "lat_print_int"
        "printString" -> "lat_print_string"
        "error"       -> "lat_error"
        _             -> i1 ++ "." ++ i2
    else i1 ++ "." ++ i2
