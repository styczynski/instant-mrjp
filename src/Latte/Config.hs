{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module Latte.Config where
    
import qualified Backend.Base as Backend
import qualified Data.Map as M
import Parser.Types
import Control.Lens
import qualified Parser.Types as ParserTypes

import qualified Reporting.Errors.Def as Error
import Reporting.Errors.Errors

import qualified Reporting.Logs as Logs
import Parser.Types (ParseError)

data CompilerInput = CompilerInput {
    _compiInputFilePath :: String
    , _compiInputFileContent :: IO String
}

data CompilerConfig a = CompilerConfig {
    _compcCompilerBackend :: Backend.LatteBackend a
    , _compcLoggingLevel :: Logs.LogLevel
}

data CompilationOutput = CompilationOutput {
    -- _compoOutputFiles :: M.Map String String
    _compoOutputExecutablePath :: String
} deriving (Show)

data CompilationError =
    ParseError ParserTypes.ParseError
    | CompilationError Error.Error
    deriving (Show)

data CompilationResult =
    CompilationFailed CompilationError String String RawProgram
    | CompilationOK CompilationOutput
    deriving (Show)


makeLensesWith abbreviatedFields ''CompilationOutput
makeLensesWith abbreviatedFields ''CompilerConfig
makeLensesWith abbreviatedFields ''CompilerInput

inputDirect :: String -> CompilerInput
inputDirect codeString = CompilerInput {
    _compiInputFilePath = "<input>"
    , _compiInputFileContent = return codeString
}

defaultConfigFor :: Backend.LatteBackend a -> CompilerConfig a
defaultConfigFor backend = CompilerConfig {
    _compcCompilerBackend = backend
    , _compcLoggingLevel = Logs.LogNothing
}