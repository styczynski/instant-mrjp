{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.ParsingFailuresSpec (spec) where

import Test.Utils.Utils
import Text.RawString.QQ
import Test.Hspec

spec :: Spec
spec = do
  describe "Compiler rejects input that contains invalid Latte syntax" $ do
    it "Main without bracket" $ \h -> expectParsingError [r|
        int main() { return 0;
      |]
    it "Single comment" $ \h -> expectParsingError [r|
        /*
      |]
    it "Single literal" $ \h -> expectParsingError [r|
        a
      |]
    it "Mismatched main brackets" $ \h -> expectParsingError [r|
        int main)( {
                return 0;
                return 1;
        }
      |]
    it "No function type" $ \h -> expectParsingError [r|
        foo() {}
      |]
    it "Variable declaration in if condition" $ \h -> expectParsingError [r|
        int main() {
            if(int i = 0) {
            }
            return 0;
        }
      |]