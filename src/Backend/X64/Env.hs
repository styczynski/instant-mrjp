{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module Backend.X64.Env where

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                      as Map
import           IR.Flow.CFG      (CFG (..), Node (..))
import           IR.Flow.Liveness
import           IR.Syntax.Syntax
import           IR.Types                (deref, isInt, isStr, ptrType,
                                                strType, valType)
import           IR.Utils                     (isPowerOfTwo, log2, single)
import           IR.CodeGen.Consts
import qualified IR.CodeGen.Emit           as Emit
import           IR.CodeGen.Epilogue
import           IR.CodeGen.Module
import           IR.CodeGen.Stack
import           IR.Class
import           IR.RegisterAllocation.RegisterAllocation

import Control.Lens
import Data.Int

import qualified Backend.X64.Parser.Constructor as X64

data VarState = VarState {
    _varstVarName :: ValIdent,
    _varstVarType :: SType (),
    _varstVarLoc  :: X64.Loc
} deriving (Show)

data CompiledMethod = CompiledMethod {
    -- Label of the method start, the target for calls.
    _cmthdEntry    :: String,
    _cmthdPrologue :: [String],
    _cmthdCode     :: [String],
    _cmthdEpilogue :: [String]
} deriving (Show)

data VarKind = VarImm
             | VarPtr Int64

data GeneratorEnv = GeneratorEnv {
    -- All code generated thus far, in reverse.
    _geeAllCode  :: [String],
    -- All code generated for the current basic block, in reverse.
    _geeCode   :: [String],
    -- All string constants used thus far.
    _geeConsts   :: ConstSet,
    -- The current state of the stack.
    _geeStack    :: Stack,
    -- Descriptions of variables.
    _geeVars     :: Map.Map ValIdent VarState,
    -- Currently live variables and their next usage.
    _geeLive     :: Liveness,
    _geeTraceIdx :: Integer -- debug
}

data GeneratorContext = GeneratorContext {
    -- Generator of labels for the current method.
    _gecLabelGen :: LabIdent -> LabIdent,
    -- Location colouring of variables.
    _gecRegs     :: RegisterAllocation,
    -- Type metadata.
    _gecClasses  :: Map.Map SymIdent CompiledClass
}

makeLensesWith abbreviatedFields ''GeneratorEnv
makeLensesWith abbreviatedFields ''GeneratorContext
makeLensesWith abbreviatedFields ''CompiledMethod
makeLensesWith abbreviatedFields ''VarState



