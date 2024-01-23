module IR.Identifiers where

import           IR.Syntax.Syntax
import qualified Linearized.BuiltIns as BuiltIns


argIRValueName :: String -> IRValueName
argIRValueName s = IRValueName $ "%a_" ++ s

constIdent :: String -> String
constIdent = ("__const_" ++)


exitLabel :: IRLabelName
exitLabel = IRLabelName ".L_exit"


indexedIRValueName :: String -> Integer -> IRValueName
indexedIRValueName i idx =
    let suf = if idx == 0 then "" else '_':show idx
    in valIdent (i ++ suf)

labIdent :: String -> IRLabelName
labIdent = IRLabelName . (".L_" ++)

phiUnfoldJumpFromToLabel :: IRLabelName -> IRLabelName -> IRLabelName
phiUnfoldJumpFromToLabel (IRLabelName from) (IRLabelName to) = IRLabelName $ to ++ "__from_" ++ trim from
    where
        trim ('.':'L':'_':xs) = xs
        trim xs               = xs


getCallTargetStr :: QIdent a -> String
getCallTargetStr ident = let (name, _, _) = getCallTarget ident in name

getCallTarget :: QIdent a -> (String, String, String)
getCallTarget (QIdent _ (IRTargetRefName i1) (IRTargetRefName i2)) =
    let defaultTarget = i1 ++ "." ++ i2 in
    case lookup defaultTarget BuiltIns.builtInsLabels of 
        Nothing -> (defaultTarget, i1, i2)
        (Just mappedTarget) -> mappedTarget
