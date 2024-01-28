module IR.Class where

import           Data.Int            (Int64)
import           Data.List           (foldl', sortBy, elemIndex)
import qualified Data.Map            as Map
import           IR.Syntax.Syntax
import IR.Identifiers
import           IR.Size         (typeSize)
import qualified Backend.X64.Parser.Constructor as X64

data CompiledClass = CompiledCl {
    clName   :: IRTargetRefName,
    clFlds   :: Map.Map IRTargetRefName CompiledField,
    clFldsLayout :: [CompiledField],
    clSize   :: Int64,
    clVTable :: VTable,
    clChain  :: [IRTargetRefName]
}

data CompiledField = Fld {
    fldName   :: IRTargetRefName,
    fldType   :: SType (),
    fldOffset :: Int64
}

data VTable = VTab {
    vtabMthds   :: [(String, Int64)],
    vtabMthdMap :: Map.Map IRTargetRefName (String, Int64)
}

