module IR.CodeGen.Module where

import           Data.List
import qualified Data.Map              as Map
import           IR.Syntax.Syntax
import           IR.Identifiers
import           IR.Class
import           IR.CodeGen.Consts
import qualified IR.CodeGen.Emit   as Emit
import           IR.CodeGen.GenM
import           IR.Size
import qualified Backend.X64.Parser.Constructor as X64

-- Combine compiled methods to produce a full assembly file as string.
generateModule :: [CompiledClass] -> [CompiledMethod] -> ConstSet -> String
generateModule cls mthds allConsts =
       let code = concatMap (\m -> emitMthd m ++ "\n") mthds
           header = map Emit.extern runtimeSymbols ++
                    [Emit.globalMain | mainEntry `elem` map mthdEntry mthds] ++
                    map Emit.constDef (constsElems allConsts)
           classComments = Emit.commentMultiline $ "Class metadata:":concatMap commentClass cls
       in unlines (map Emit.emitAsString $ header ++ (classComments:concatMap vtable cls)) ++
          "\n" ++ unlines (map Emit.emitAsString nullRef) ++
          "\n\n" ++ code
    where mainEntry = toStr $
            labelFor (QIdent () (IRTargetRefName "~cl_TopLevel") (IRTargetRefName "main")) entryLabel
          emitMthd mthd =
              let code = unlines $ mthdPrologue mthd ++ mthdCode mthd ++ mthdEpilogue mthd
              in if mthdEntry mthd == mainEntry then "main:\n" ++ code else code
          commentClass cl = ("Class " ++ toStr (clName cl) ++ ":"):
                             "  Fields:":concatMap commentField (sortOn fldOffset $ Map.elems $ clFlds cl)
          commentField fld = ["    Field name:   " ++ toStr (fldName fld),
                              "    Field type:   " ++ show (fldType fld),
                              "    Field offset: " ++ show (fldOffset fld),
                              "    Field size:   " ++ show (X64.toBytes $ typeSize $ fldType fld)]
          vtable cl = if toStr (clName cl) == "~cl_TopLevel" then []
                      else [Emit.global (classDefIdent (clName cl))
                            ,Emit.global (vTableIRLabelName (clName cl))
                            ,Emit.label (classDefIdent (clName cl)) ""
                            ,Emit.quadDef (let (IRLabelName l) = (if length (clChain cl) == 1 then (IRLabelName "0") else classDefIdent ((clChain cl)!!1)) in l) -- parent
                            ,Emit.longDef (show $ clSize cl)
                            ,Emit.quadDef (let (IRLabelName l) = vTableIRLabelName (clName cl) in l)
                            ,Emit.longDef "0"
                            ,Emit.quadDef "0"
                            ,Emit.label (vTableIRLabelName (clName cl)) ""]++map (Emit.quadDef . fst) (vtabMthds $ clVTable cl)
          nullRef = [Emit.label nullrefLabel "runtime error on null dereference",
                     Emit.and X64.Size64 (X64.LocConst (-16)) (X64.LocReg X64.RSP) "16 bytes allign",
                     Emit.callDirect "__errorNull"]
