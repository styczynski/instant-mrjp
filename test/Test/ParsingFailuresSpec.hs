{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.ParsingFailuresSpec (spec) where

import Text.RawString.QQ
import           Test.Hspec
import           System.IO

import qualified Latte.Compiler as Compiler
import qualified Backend.X64.X64 as BackendX64

import Control.Lens

checkParseError (Compiler.CompilationFailed (Compiler.ParseError _) _ _ _) = True
checkParseError _ = False

tryParsing code = (`shouldSatisfy` checkParseError) =<< (Compiler.compileLatte (Compiler.defaultConfigFor BackendX64.backend) (Compiler.inputDirect code))

spec :: Spec
spec = do
  describe "Compiler rejects input that contains invalid Latte syntax" $ do
    it "Main without bracket" $ \h -> tryParsing [r|
        int main() { return 0;
      |]
    it "Single comment" $ \h -> tryParsing [r|
        /*
      |]
    it "Single literal" $ \h -> tryParsing [r|
        a
      |]
    it "Mismatched main brackets" $ \h -> tryParsing [r|
        int main)( {
                return 0;
                return 1;
        }
      |]
    it "No function type" $ \h -> tryParsing [r|
        foo() {}
      |]
    it "Variable declaration in if condition" $ \h -> tryParsing [r|
        int main() {
            if(int i = 0) {
            }
            return 0;
        }
      |]