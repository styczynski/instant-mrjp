{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.EmptySpec (spec) where

import Text.RawString.QQ
import           Test.Hspec
import           System.IO

import qualified Latte.Compiler as Compiler
import qualified Backend.X64.X64 as BackendX64

import Control.Lens

code :: String
code = [r|
  int main() { return 0;
|]

spec :: Spec
spec = do
  describe "hGetLine" $ do
    it "reads a line" $ \h -> do
      r <- Compiler.compileLatte (Compiler.defaultConfigFor BackendX64.backend) (Compiler.inputDirect code)
      (r `shouldSatisfy` (\case
           (Compiler.CompilationFailed (Compiler.ParseError _) _ _ _) -> True
           _ -> False))
      -- (r `shouldSatisfy` (\case
      --      (Compiler.CompilationOK _) -> True
      --      _ -> False))
      -- case result of 
      --   (Compiler.CompilationFailed err _ _ _) -> err `shouldBe`
      --   (Compiler.CompilationOK result) -> (result ^. Compiler.outputExecutablePath)