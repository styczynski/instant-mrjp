{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.BranchingSpec (spec) where

import Test.Utils.Utils
import Text.RawString.QQ
import Test.Hspec

spec :: Spec
spec = do
  describe "Latte branching and block contexts" $ do

    it "Tricky expression nested context" $ \h -> expectProgramSuccess [r|
        int main() {
          int i = 78;
          {
            int i = 1;
            printInt(i);
          }
          printInt(i);
          while (i > 76) {
            i--;
            printInt(i);
          // this is a little tricky
          // on the right hand side, i refers to the outer i
          int i = i + 7;
          printInt(i);
          }
          printInt(i);
          if (i > 4) {
            int i = 4;
            printInt(i);
          } else {
            printString("foo");
          } 
          printInt(i);
          return 0 ;
        }
    |] [r|
        1
        78
        77
        84
        76
        83
        76
        4
        76
    |]


    it "Simplifying trivial if branches with returns" $ \h -> expectProgramSuccess [r|
        int f () {
          if (true)
            return 0;
          else
            {}
        }
        int g () {
          if (false) 
              {}
          else
              return 0;
        }
        void p () {}
        int main() {
          p();
          return 0;
        }
    |] [r|

    |]

    it "Simple print in trivial if" $ \h -> expectProgramSuccess [r|
        int main() {
            if (true) {
              printInt(1);
              return 0;
            }
        }
    |] [r|
        1
    |]

    it "Usage of variable initialized in both branches" $ \h -> expectProgramSuccess [r|
        int main () {
          int x;
          int y = 56;
          if (y + 45 <= 2) {
            x = 1;
          } else {
            x = 2;
          }
          printInt(x);
          return 0 ;
        }
    |] [r|
        2
    |]

    it "Simple nested block variable context handling" $ \h -> expectProgramSuccess [r|
        int main() {
            int i = 0;
            printInt(i);
            {
                int i = 1;
                printInt(i);
            }
            printInt(i);
            {
                int i = 2;
                printInt(i);
            }
            printInt(i);
            return 0;
        }
    |] [r|
        0
        1
        0
        2
        0
    |]

    it "Empty if statement" $ \h -> expectProgramSuccess [r|
        int main() {
            if(false);
            printInt(1);
            return 0;
        }
    |] [r|
        1
    |]

    it "Simple interactive infinite loop" $ \h -> expectProgramSuccessIn ["0", "1"] [r|
        int main() {
            while (true) {
                int x;
                x = readInt();
                if (x == 1)
                    return 0;
                else
                    printString("jeszcze raz");
            }
        }
    |] [r|
        jeszcze raz
    |]

    it "Infinite loop immediate exit by return" $ \h -> expectProgramSuccess [r|
        int main() {
            while(true) {
                return 0;
            }
        }
    |] [r|

    |]

    it "Empty while block" $ \h -> expectProgramSuccess [r|
        int main() {
            while(false);
            printInt(1);
            return 0;
        }
    |] [r|
        1
    |]