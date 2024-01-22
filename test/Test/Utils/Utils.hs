module Test.Utils.Utils where

import Control.Monad
import Control.Monad.IO.Class
import System.Random
import Control.Lens
import Text.RawString.QQ
import           Test.Hspec
import           System.IO
import System.Process
import System.Directory
import GHC.IO.Exception
import qualified Latte.Compiler as Compiler
import qualified Backend.X64.X64 as BackendX64
import qualified Reporting.Errors.Def as Errors
import qualified Program.Syntax as Syntax
import qualified Typings.Types as Type
import qualified Data.Text as T

_inputUID = liftM (take 10 . randomRs ('a','z')) newStdGen
_testDebug = liftIO . hPutStrLn stderr

checkParseError (Compiler.CompilationFailed (Compiler.ParseError _) _ _ _) = True
checkParseError _ = False

checkOutputError fn (Compiler.CompilationFailed (Compiler.CompilationError err) _ _ _) = fn err
checkOutputError _ _ = False

checkOK (Compiler.CompilationOK compilationOut) = True
checkOK _ = False

expectParsingError code = (`shouldSatisfy` checkParseError) =<< (\uid -> Compiler.compileLatte (Compiler.defaultConfigFor BackendX64.backend) (Compiler.inputDirect uid code)) =<< _inputUID
expectCheckerError fn code = (`shouldSatisfy` (checkOutputError fn)) =<< (\uid -> Compiler.compileLatte (Compiler.defaultConfigFor BackendX64.backend) (Compiler.inputDirect uid code)) =<< _inputUID

expectProgramSuccess :: String -> String -> Expectation
expectProgramSuccess code expectedOut = expectProgramSuccessIn [] code expectedOut

expectProgramSuccessIn :: [String] -> String -> String -> Expectation
expectProgramSuccessIn givenInput code expectedOut = do
  uid <- _inputUID
  let config = (Compiler.defaultConfigFor BackendX64.backend) & Compiler.outputDirectory .~ (Just "./_test_temp_")
  result <- Compiler.compileLatte config (Compiler.inputDirect uid code)
  result `shouldSatisfy` checkOK
  let (Compiler.CompilationOK compilationOut) = result
  let executablePathRel = compilationOut ^. Compiler.outputExecutablePath
  executablePath <- makeAbsolute executablePathRel
  exists <- doesFileExist executablePath
  case exists of
    True -> return ()
    False -> do
      _testDebug $ "executeCompiledProgram: Compiled program does not exist " ++ (show executablePath)
      exists `shouldBe` True
  (Just inp, Just out, Just outErr, phandle) <- createProcess_ "executeCompiledProgram" (proc executablePath []){ std_err = CreatePipe, std_out = CreatePipe, std_in = CreatePipe }
  mapM_ (hPutStrLn inp) givenInput
  hClose inp
  ph <- waitForProcess phandle
  let normalizeOut = filter (not . T.isPrefixOf (T.pack "~#LATCINSTR#~")) . filter (not . T.null) . map T.stripStart . T.lines . T.pack
  outErrStr <- hGetContents outErr
  outStr <- hGetContents out
  let (outText, expectedText, errText) = (normalizeOut outStr, normalizeOut expectedOut, normalizeOut outErrStr)
  wrapStatusCheck executablePath ph (T.unpack $ T.unlines errText)
  outText `shouldBe` expectedText
  return ()
  where
    wrapStatusCheck filePath ph outErrStr = do
      case ph of
        ExitSuccess   -> return ()
        ExitFailure fail -> do
          _testDebug $ "executeCompiledProgram: Failed to execute the compiled program " ++ (show filePath) ++ ": " ++ (show fail)
          _testDebug $ outErrStr
          ph `shouldBe` ExitSuccess