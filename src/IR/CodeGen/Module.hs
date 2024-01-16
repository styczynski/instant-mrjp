module IR.CodeGen.Module where

import           Data.List
import qualified Data.Map              as Map
import           IR.Syntax.Syntax
import           IR.Identifiers
import           IR.Class
import           IR.CodeGen.Consts
import qualified IR.CodeGen.Emit   as Emit
import           IR.CodeGen.GenM
import           IR.Loc
import           IR.Registers
import           IR.Size

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
            labelFor (QIdent () (SymIdent "~cl_TopLevel") (SymIdent "main")) entryLabel
          emitMthd mthd =
              let code = unlines $ mthdPrologue mthd ++ mthdCode mthd ++ mthdEpilogue mthd
              in if mthdEntry mthd == mainEntry then "main:\n" ++ code else code
          commentClass cl = ("Class " ++ toStr (clName cl) ++ ":"):
                             "  Fields:":concatMap commentField (sortOn fldOffset $ Map.elems $ clFlds cl)
          commentField fld = ["    Field name:   " ++ toStr (fldName fld),
                              "    Field type:   " ++ show (fldType fld),
                              "    Field offset: " ++ show (fldOffset fld),
                              "    Field size:   " ++ show (sizeInBytes $ typeSize $ fldType fld)]
          vtable cl = if toStr (clName cl) == "~cl_TopLevel" then []
                      else [Emit.global (classDefIdent (clName cl))
                            ,Emit.global (vTableLabIdent (clName cl))
                            ,Emit.label (classDefIdent (clName cl)) ""
                            ,Emit.quadDef (let (LabIdent l) = classDefIdent (clName cl) in l) -- parent
                            ,Emit.longDef (show $ clSize cl)
                            ,Emit.quadDef (let (LabIdent l) = vTableLabIdent (clName cl) in l)
                            ,Emit.longDef "0"
                            ,Emit.quadDef "0"
                            ,Emit.label (vTableLabIdent (clName cl)) ""]++map (Emit.quadDef . fst) (vtabMthds $ clVTable cl)
          nullRef = [Emit.label nullrefLabel "runtime error on null dereference",
                     Emit.and Quadruple (LocImm (-16)) (LocReg rsp) "16 bytes allign",
                     Emit.callDirect "__errorNull"]
