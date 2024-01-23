{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Latte.Compiler(
    compileLatte
    , compileLatte
    , compileLatteThen
    , printCompilerErrors
    , module Latte.Config
) where

import Control.Lens

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.List as L
import qualified Data.Text as T

import Reporting.Logs
import Parser.Parser
import qualified Parser.Types as ParserTypes
import Parser.Transform
import Typings.Checker as TypeChecker
import Optimizer.Optimizer as Optimizer
import Linearized.Linearizer as Linearizer
import qualified Backend.Base as Backend
import qualified Backend.X64.X64 as BackendX64
import qualified Reporting.Errors.Errors as ErrorsUtils
import qualified Linearized.Syntax as LSyntax

import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process
import System.Directory
import Control.Monad

import Utils.Similarity

import IR.Syntax.Print(printTree)
import qualified IR.Compl as Compl

import Latte.Config

printCompilerErrors :: CompilationError -> String -> String -> ParserTypes.RawProgram -> LattePipeline ()
printCompilerErrors (ParseError err) filePath contents rawProgram = ErrorsUtils.printErrors err filePath contents rawProgram
printCompilerErrors (CompilationError err) filePath contents rawProgram = ErrorsUtils.printErrors err filePath contents rawProgram

compileLatteThen :: CompilerConfig LSyntax.IRPosition -> CompilerInput h -> (CompilationResult -> LattePipeline t) -> IO t
compileLatteThen config input fn = evaluateLattePipeline (config ^. loggingLevel) (fn =<< compileLattePipeline config input)

compileLatte :: CompilerConfig LSyntax.IRPosition -> CompilerInput h -> IO CompilationResult
compileLatte config input = compileLatteThen config input (return)

compileLattePipeline :: CompilerConfig LSyntax.IRPosition -> CompilerInput h -> LattePipeline CompilationResult
compileLattePipeline config input = do
    let file = input ^. inputFilePath
    let usedBackend = config ^. compilerBackend
    printLogInfo $ "Reading: " <> (T.pack file)
    (contents, contentsHandle) <- liftIO $ input ^. inputFileContent
    printLogInfo $ "Parsing: " <> (T.pack file)
    parsedAST <- parseLatte file contents
    case parsedAST of 
        p@(ParserTypes.ProgramParseError err) -> do
            liftIO $ (input ^. inputFileContentClose) contentsHandle
            return $ CompilationFailed (ParseError err) file contents p
        ast -> do
            printLogInfo $ "Parsed: " <> (T.pack file)
            prog <- transformAST file ast
            printLogInfo $ "Transformed: " <> (T.pack file) 
            typingResult <- TypeChecker.checkTypes prog
            case typingResult of
                Left err -> do
                    liftIO $ (input ^. inputFileContentClose) contentsHandle
                    return $ CompilationFailed (CompilationError err) file contents parsedAST
                Right (tcEnv, prog', _) -> do
                    printLogInfo $ "Typecheck done" <> (T.pack file) <> "\n\n" <> (T.pack $ printi 0 prog')
                    optimizerResult <- Optimizer.optimize tcEnv prog'
                    case optimizerResult of
                        Left err -> do
                            liftIO $ (input ^. inputFileContentClose) contentsHandle
                            return $ CompilationFailed (CompilationError err) file contents parsedAST
                        Right (optimizerEnv, prog'') -> do
                            printLogInfo $ "Optimization done" <> (T.pack file) <> "\n\n" <> (T.pack $ printi 0 prog'')
                            irResult <- Linearizer.linearizeToIR optimizerEnv prog''
                            case irResult of
                                Left err -> do
                                    liftIO $ (input ^. inputFileContentClose) contentsHandle
                                    return $ CompilationFailed (CompilationError err) file contents parsedAST
                                Right (_, ir) -> do
                                    printLogInfo $ "IR conversion done" <> (T.pack file) <> "\n\n" <> (T.pack $ printTree ir)
                                    compiledProg <- ((Compl.compl_ ir) :: (LattePipeline (Compl.CompiledProg LSyntax.IRPosition))) --(fmap (const ()) ir)
                                    printLogInfo $ "COMPL_ DONE" <> (T.pack file) <> "\n\n" <> (T.pack $ show compiledProg)
                                    outputPath <- case (config ^. outputDirectory) of
                                        (Just outputPath) -> do
                                            let (_, inputFileName) = splitFileName file
                                            liftIOToPipeline $ createDirectoryIfMissing True outputPath
                                            return $ replaceExtension (outputPath </> inputFileName) (Backend.inputExtension usedBackend)
                                        Nothing -> return $ replaceExtension file (Backend.inputExtension usedBackend)
                                    backendResult <- Backend.runBackend outputPath (takeFileName outputPath) compiledProg usedBackend
                                    case backendResult of 
                                        (Left err) -> do
                                            printLogInfo $ T.pack $ "Backend code generation has failed"
                                            liftIO $ (input ^. inputFileContentClose) contentsHandle
                                            return $ CompilationFailed (CompilationError err) file contents parsedAST
                                        (Right (_, _, outputFilePath)) -> do
                                            printLogInfo $ "Backend code generation completed successfully " <> (T.pack file) <> " -> " <> (T.pack outputFilePath)
                                            liftIO $ (input ^. inputFileContentClose) contentsHandle
                                            return $ CompilationOK $ CompilationOutput { _compoOutputExecutablePath = outputFilePath }
