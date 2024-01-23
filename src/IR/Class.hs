module IR.Class where

import           Data.Int            (Int64)
import           Data.List           (foldl', sortBy)
import qualified Data.Map            as Map
import           IR.Syntax.Syntax
import IR.Identifiers
import           IR.Size         (typeSize)

import qualified Backend.X64.Parser.Constructor as X64

data CompiledClass = CompiledCl {
    clName   :: SymIdent,
    clFlds   :: Map.Map SymIdent CompiledField,
    clSize   :: Int64,
    clVTable :: VTable,
    clChain  :: [SymIdent]
}

data CompiledField = Fld {
    fldName   :: SymIdent,
    fldType   :: SType (),
    fldOffset :: Int64
}

data VTable = VTab {
    vtabMthds   :: [(String, Int64)],
    vtabMthdMap :: Map.Map SymIdent (String, Int64)
}

compileClass :: ClassDef a -> CompiledClass
compileClass (ClDef _ i chain fldDefs mthdDefs) =
    let (flds, unalignedSize) = layoutFields fldDefs
        vTable = generateVTable mthdDefs
    in  CompiledCl i (Map.fromList $ map (\f -> (fldName f, f)) flds) (alignSize unalignedSize) vTable chain

alignSize :: Int64 -> Int64
alignSize n = if n `mod` 8 == 0
                then n
                else n + (8 - n `mod` 8)

layoutFields :: [FieldDef a] -> ([CompiledField], Int64)
layoutFields fldDefs =
    let fldBase = map (\(FldDef _ t sym) -> Fld sym (() <$ t) 0) fldDefs
    in  foldl' go ([], 0) fldBase
    where
        go (flds, offset) fld =
            let fldSize = X64.toBytes (typeSize (fldType fld))
                padding = if offset `mod` fldSize == 0
                            then 0
                            else fldSize - (offset `mod` fldSize)
            in (fld{fldOffset = offset + padding}:flds, offset + padding + fldSize)

getVTableRelPos :: String -> String -> Int
getVTableRelPos _ "equals" = 3
getVTableRelPos _ "getHashCode" = 2
getVTableRelPos _ "toString" = 1
getVTableRelPos _ _ = 100

generateVTable :: [MethodDef a] -> VTable
generateVTable mthdDefs =
    let lookupList = zipWith (\(MthdDef _ _ qi@(QIdent _ _ si)) idx -> let (ct, cls, mtd) = getCallTarget qi in (si, (getVTableRelPos cls mtd, ct, 0))) mthdDefs [0..] in
    let lookupList' = map (\(newIndex, (si, (score, name, _))) -> (si, (name, newIndex * 8))) $ zip [0..] $ sortBy (\(si, (score, name, offset)) (si2, (score2, name2, offset2)) -> compare score score2) lookupList
    in  VTab (map snd lookupList') (Map.fromList lookupList')
