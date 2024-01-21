{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.TypecheckingFailuresSpec (spec) where

import Text.RawString.QQ
import           Test.Hspec
import           System.IO

import qualified Latte.Compiler as Compiler
import qualified Backend.X64.X64 as BackendX64
import qualified Reporting.Errors.Def as Errors
import qualified Program.Syntax as Syntax
import qualified Typings.Types as Type

import Control.Lens

checkOutputError fn (Compiler.CompilationFailed (Compiler.CompilationError err) _ _ _) = fn err
checkOutputError _ _ = False

tryParsing fn code = (`shouldSatisfy` (checkOutputError fn)) =<< (Compiler.compileLatte (Compiler.defaultConfigFor BackendX64.backend) (Compiler.inputDirect code))

spec :: Spec
spec = do
  describe "Compiler rejects input that was determined to by semantically incorrect" $ do
    it "Unknown variable" $ \h -> tryParsing (\case { (Errors.UnknownVariable _ (Syntax.Ident _ name)) | name == "x" -> True; _ -> False }) [r|
        int main() {
            x = 14;
            return 0 ;
        }
      |]
    it "Repeated function parameter name" $ \h -> tryParsing (\case { (Errors.DuplicateFunctionArgument _ (Type.Fun (Syntax.Ident _ fnName) _ _ _) (Syntax.Arg _ _ (Syntax.Ident _ varName)) _) | fnName == "f" && varName == "x" -> True; _ -> False }) [r|
        int f(int x, int x) {
          return x;
        }
        int main() { return 0; }
      |]
    it "Repeated variable declaration" $ \h -> tryParsing (\case { (Errors.VariableRedeclared _ _ (Syntax.Ident _ name1, Syntax.IntT _) (Syntax.Ident _ name2, Syntax.IntT _)) | name1 == name2 && name2 == "x" -> True; _ -> False }) [r|
        int main() {
          int x;
          int x;
          return 0 ;
        }
      |]
    it "Missing main() return" $ \h -> tryParsing (\case { (Errors.FunctionLacksReturn _ _ (Type.Fun (Syntax.Ident _ fnName) _ _ _) _) | fnName == "main" -> True; _ -> False }) [r|
          int main() { 
              if (false)
                return 0; 
          }
      |]
    it "Incompatible types assignment" $ \h -> tryParsing (\case { (Errors.IncompatibleTypesAssign _ _ (Syntax.BoolT _) (Syntax.IntT _)) -> True; _ -> False }) [r|
          int main() {
              int x;
              x = true;
              return 1;
          }
      |]
    it "Return without value in main()" $ \h -> tryParsing (\case { (Errors.MissingReturnValue _ _ (Syntax.IntT _) (Type.Fun (Syntax.Ident _ fnName) _ _ _)) | fnName == "main" -> True; _ -> False }) [r|
          int main() {
              if (true)
                  return;
              ;
              return 1;
          }
      |]
    it "Invalid return value type in main()" $ \h -> tryParsing (\case { (Errors.IncompatibleTypesReturn _ _ (Type.Fun (Syntax.Ident _ fnName) _ _ _) (Syntax.BoolT _) (Syntax.IntT _))| fnName == "main" -> True; _ -> False }) [r|
          int main() {
              return true;
          }
      |]
    it "No return in boolean function foo()" $ \h -> tryParsing (\case { (Errors.FunctionLacksReturn _ _ (Type.Fun (Syntax.Ident _ fnName) _ _ _) _) | fnName == "foo" -> True; _ -> False }) [r|
          int main() {
              int i = foo(true);
              return 0 ;
          }

          int foo(boolean b) { b = true; }
      |]
    -- This should be bad?
    --     int main() {
    --       string x ;
    --       x = "pi" + 1 ;
    --       return 0 ;
    -- }
    it "Invalid parameter type in printInt(string)" $ \h -> tryParsing (\case { (Errors.MissingReturnValue _ _ (Syntax.IntT _) (Type.Fun (Syntax.Ident _ fnName) _ _ _)) | fnName == "foo" -> True; _ -> False }) [r|
          int main() {
            printInt("foo");
            return 0 ;
          }
      |]
