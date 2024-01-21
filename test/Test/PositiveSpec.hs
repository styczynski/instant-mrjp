{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.PositiveSpec (spec) where

import Test.Utils.Utils
import Text.RawString.QQ
import Test.Hspec

spec :: Spec
spec = do
  describe "Positive examples" $ do
    it "Hello world" $ \h -> expectProgramSuccess [r|
        int main() {
            printString("Hello world");
            return 0;
        }
      |] [r|
        Hello world
      |]
    