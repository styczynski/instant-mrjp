{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module Latte.Config where
    
import qualified Backend.Base as Backend
import qualified Data.Map as M
import Parser.Types
import Control.Lens
import System.IO
import qualified Parser.Types as ParserTypes

import qualified Reporting.Errors.Def as Error
import Reporting.Errors.Errors

import qualified Reporting.Logs as Logs
import Parser.Types (ParseError)

data CompilerInput h = CompilerInput {
    _compiInputFilePath :: String
    , _compiInputFileContent :: IO (String, h)
    , _compiInputFileContentClose :: h -> IO ()
}

data CompilerConfig a = CompilerConfig {
    _compcCompilerBackend :: Backend.LatteBackend a
    , _compcLoggingLevel :: Logs.LogLevel
    , _compcOutputDirectory :: Maybe String
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

inputDirect :: String -> String -> CompilerInput ()
inputDirect uid codeString = CompilerInput {
    _compiInputFilePath = "<input:" ++ uid ++ ">"
    , _compiInputFileContent = return (codeString, ())
    , _compiInputFileContentClose = const $ return ()
}

inputFile :: FilePath -> CompilerInput Handle
inputFile filePath = CompilerInput {
    _compiInputFilePath = filePath
    , _compiInputFileContent = do
        inputHandle <- openFile filePath ReadMode 
        hSetEncoding inputHandle utf8
        theInput <- hGetContents inputHandle
        return (theInput, inputHandle)
    , _compiInputFileContentClose = hClose
}

defaultConfigFor :: Backend.LatteBackend a -> CompilerConfig a
defaultConfigFor backend = CompilerConfig {
    _compcCompilerBackend = backend
    , _compcLoggingLevel = Logs.LogNothing
    , _compcOutputDirectory = Nothing
}