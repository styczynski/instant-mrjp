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
    _varstVarName :: IRValueName,
    _varstVarType :: SType (),
    _varstVarLoc  :: X64.Loc
} deriving (Show)

data CompiledMethod = CompiledMethod {
    -- Label of the method start, the target for calls.
    _cmthdEntry    :: String,
    _cmthdPrologue :: [String],
    _cmthdCode     :: String,
    _cmthdEpilogue :: [String]
} deriving (Show)

data VarKind = VarImm
             | VarPtr Int64

data GeneratorEnv = GeneratorEnv {
    _geeAllCode  :: [String],
    _geeCode   :: [String],
    _geeConsts   :: ConstSet,
    _geeStack    :: Stack,
    _geeVars     :: Map.Map IRValueName VarState,
    _geeLive     :: Liveness,
    _geeTraceIdx :: Integer
}

data GeneratorContext = GeneratorContext {
    _gecLabelGen :: IRLabelName -> IRLabelName,
    _gecRegs     :: RegisterAllocation,
    _gecClasses  :: Map.Map IRTargetRefName CompiledClass
}


emptyGeneratorEnv :: GeneratorEnv
emptyGeneratorEnv = GeneratorEnv {
    _geeAllCode = []
    , _geeCode = []
    , _geeConsts = constsEmpty
    , _geeStack = stackNew 0
    , _geeVars = Map.empty
    , _geeLive = emptyLiveness
    , _geeTraceIdx = 0
}

emptyGeneratorContext :: GeneratorContext
emptyGeneratorContext = GeneratorContext {
    _gecLabelGen = id
    , _gecRegs = emptyRegisterAllocation
    , _gecClasses = Map.empty
}

makeLensesWith abbreviatedFields ''GeneratorEnv
makeLensesWith abbreviatedFields ''GeneratorContext
makeLensesWith abbreviatedFields ''CompiledMethod
makeLensesWith abbreviatedFields ''VarState



