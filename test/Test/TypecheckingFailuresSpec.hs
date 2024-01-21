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
    it "Missing main() return after condition" $ \h -> tryParsing (\case { (Errors.FunctionLacksReturn _ _ (Type.Fun (Syntax.Ident _ fnName) _ _ _) _) | fnName == "main" -> True; _ -> False }) [r|
          int main() { 
              if (false)
                return 0; 
          }
      |]
    it "Missing f() return after condition" $ \h -> tryParsing (\case { (Errors.FunctionLacksReturn _ _ (Type.Fun (Syntax.Ident _ fnName) _ _ _) _) | fnName == "f" -> True; _ -> False }) [r|
          int main() {
            return f(3); 
          }

          int f(int x) {
              if (x<0) 
                return x;
          }
      |]
    it "Missing main() return" $ \h -> tryParsing (\case { (Errors.FunctionLacksReturn _ _ (Type.Fun (Syntax.Ident _ fnName) _ _ _) _) | fnName == "main" -> True; _ -> False }) [r|
          int main() {}
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
    it "Invalid parameter type in printInt(string)" $ \h -> tryParsing (\case { (Errors.CallInvalidParameterType _ (Syntax.App _ _ _) _ 1 (Syntax.StringT _) (Syntax.IntT _))  -> True; _ -> False }) [r|
          int main() {
            printInt("foo");
            return 0 ;
          }
      |]
    it "Invalid parameter type in printString(int)" $ \h -> tryParsing (\case { (Errors.CallInvalidParameterType _ (Syntax.App _ _ _) _ 1 (Syntax.ByteT _) (Syntax.StringT _))  -> True; _ -> False }) [r|
          int main() {
            printString(1);
            return 0 ;
          }
      |]
    it "Missing first parameter in the call" $ \h -> tryParsing (\case { (Errors.CallIncompatibleNumberOfParameters _ _ (Syntax.FunT _ (Syntax.IntT _) [Syntax.IntT _]) []) -> True; _ -> False }) [r|
          // 0 instead of 1 argument
          int main() {
            int x = foo();
            return 0 ;
          }

          int foo(int y) {
            return y;
          }
      |]
    it "Function call 1 instead of 2 arguments" $ \h -> tryParsing (\case { (Errors.CallIncompatibleNumberOfParameters _ _ (Syntax.FunT _ (Syntax.IntT _) [Syntax.IntT _, Syntax.IntT _]) [_]) -> True; _ -> False }) [r|
          // 1 instead of 2 arguments
          int main() {
            int x = foo(1);
            return 0 ;
          }

          int foo(int y,int z) {
            return y;
          }
      |]
    it "Function call 2 instead of 1 argument" $ \h -> tryParsing (\case { (Errors.CallIncompatibleNumberOfParameters _ _ (Syntax.FunT _ (Syntax.IntT _) [Syntax.IntT _]) [_, _]) -> True; _ -> False }) [r|
          // 2 instead of 1 arguments
          int main() {
            int x = foo(1,2);
            return 0 ;
          }

          int foo(int y) {
            return y;
          }
      |]
    it "String-boolean comparison" $ \h -> tryParsing (\case { (Errors.IncompatibleTypesBinaryOp _ (Syntax.Equ _) (_, Syntax.StringT _) (_, Syntax.BoolT _)) -> True; _ -> False }) [r|
          // Compare string with boolean.
          int main() {
            if ("true" == true) {
              printString("foo");
            }
            return 0 ;
          }
      |]
    it "Assign trivial string to int variable in initializer" $ \h -> tryParsing (\case { (Errors.IncompatibleTypesInit _ (Syntax.Init _ _ _) (Syntax.StringT _) (Syntax.IntT _)) -> True; _ -> False }) [r|
          int main() {
             int x = "";
             return 0 ;
          }
      |]
    it "Assign string concat to int variable" $ \h -> tryParsing (\case { (Errors.IncompatibleTypesAssign _ (Syntax.Assignment _ _ _) (Syntax.StringT _) (Syntax.IntT _)) -> True; _ -> False }) [r|
          int main () {
            int x;
            x = "foo"+"bar";
            return 0 ;
          }
      |]
    it "Assign trivial int to string variable in initializer" $ \h -> tryParsing (\case { (Errors.IncompatibleTypesInit _ (Syntax.Init _ _ _) (Syntax.ByteT _) (Syntax.StringT _)) -> True; _ -> False }) [r|
          int main() {
             string x = 1;
             return 0 ;
          }
      |]
    it "Assign trivial int to string variable" $ \h -> tryParsing (\case { (Errors.IncompatibleTypesAssign _ (Syntax.Assignment _ _ _) (Syntax.ByteT _) (Syntax.StringT _)) -> True; _ -> False }) [r|
          int main () {
            string x;
            x = 1;
            return 0 ;
          }
      |]
    it "Numeric constant initializer overflow" $ \h -> tryParsing (\case { (Errors.NumericConstantExceedsTypeLimit _ (Syntax.Int _ _) 999999999999999999999999999999999999999999999999999999999999 _) -> True; _ -> False }) [r|
          int main() {
            int i = 999999999999999999999999999999999999999999999999999999999999;
            return 0;
        }
      |]
    it "Class field type overload" $ \h -> tryParsing (\case { (Errors.DuplicateMembersInChain (Type.Class (Syntax.Ident _ "A") _ _ _) (Type.Field (Syntax.Ident _ "x") _ _) _) -> True; _ -> False }) [r|
        class A {
            int x;
        }
        class B extends A {
            int x;
        }

        int main() {
            return 0;
        }
      |]
    it "Void in field parameter declaration" $ \h -> tryParsing (\case { (Errors.IllegalTypeUsed _ (Errors.TypeInFunctionArgDecl (Syntax.FunctionDef _ _ (Syntax.Ident _ "f") _ _) _) (Syntax.VoidT _)) -> True; _ -> False }) [r|
          int main() {
            return 0;
          }

          int f(void x) {
            return 2;
          }
      |]
    it "Void in variable declaration" $ \h -> tryParsing (\case { (Errors.IllegalTypeUsed _ (Errors.TypeInVarDecl (Syntax.NoInit _ (Syntax.Ident _ "x"))) (Syntax.VoidT _)) -> True; _ -> False }) [r|
          int main() {
            void x;
            return 0;
          }
      |]
    it "main() has parameters" $ \h -> tryParsing (\case { (Errors.MainHasArgs (Type.Fun (Syntax.Ident _ "main") _ _ _) _) -> True; _ -> False }) [r|
          int main(int y) {
            return 0 ;
          }
      |]
    it "main() has void type" $ \h -> tryParsing (\case { (Errors.InvalidMainReturn (Type.Fun (Syntax.Ident _ "main") _ _ _) _) -> True; _ -> False }) [r|
          void main() {}
      |]
    it "main() is missing" $ \h -> tryParsing (\case { (Errors.NoMain _) -> True; _ -> False }) [r|
          int f() {
              return 0;
          }
      |]
    it "Redefined foo() with different params and return types" $ \h -> tryParsing (\case { (Errors.DuplicateFun (Type.Fun (Syntax.Ident _ "foo") _ _ _) [(Type.Fun (Syntax.Ident _ "foo") _ _ _)]) -> True; _ -> False }) [r|
          int main() {
            foo();
            return 0 ;
          }

          void foo() {
            printString("foo");
            return;
          }

          int foo(int x) {
              x = x + 1;
              return x - 5;
          }
      |]
    it "Redefined builtin function printInt(int)" $ \h -> tryParsing (\case { (Errors.DuplicateFun (Type.Fun (Syntax.Ident _ "printInt") _ _ _) [(Type.Fun (Syntax.Ident _ "printInt") _ _ _)]) -> True; _ -> False }) [r|
          int main() {
            return 0; 
          }
          void printInt(int x) {}
      |]
    it "Return void value in void-returning function" $ \h -> tryParsing (\case { (Errors.IllegalTypeUsed _ (Errors.TypeInReturn (Syntax.ReturnValue _ _)) (Syntax.VoidT _)) -> True; _ -> False }) [r|
          int main() {
              return 0;
          }

          void f() {
              return g();
          }

          void g() {
          }
      |]
    it "Decrement string" $ \h -> tryParsing (\case { (Errors.IncompatibleTypesBinaryOp _ (Syntax.Sub _) (_, Syntax.StringT _) (_, Syntax.ByteT _)) -> True; _ -> False }) [r|
          int main() {
              string a;
              a--;
              return 0;
          }
      |]
    it "Increment string" $ \h -> tryParsing (\case { (Errors.IncompatibleTypesBinaryOp _ (Syntax.Add _) (_, Syntax.StringT _) (_, Syntax.ByteT _)) -> True; _ -> False }) [r|
          int main() {
              string a = "";
              a++;
              return 0;
          }
      |]
    it "Substract string" $ \h -> tryParsing (\case { (Errors.IncompatibleTypesUnaryOp _ (Syntax.Neg _) (_, Syntax.StringT _)) -> True; _ -> False }) [r|
          int main() {
            string x;
            x = "asd" - "ewq";
            return 0;
          }
      |]
    it "Unknown variable as instruction" $ \h -> tryParsing (\case { (Errors.UnknownVariable _ (Syntax.Ident _ "i")) -> True; _ -> False }) [r|
        int main() {
            i;
            return 0 ;
        }
      |]