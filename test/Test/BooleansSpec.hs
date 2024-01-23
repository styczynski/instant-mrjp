{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.BooleansSpec (spec) where

import Test.Utils.Utils
import Text.RawString.QQ
import Test.Hspec

spec :: Spec
spec = do
  describe "Latte logical conditions" $ do
    it "Simple nested boolean condition" $ \h -> expectProgramSuccess [r|
        int main()
        {
          printInt(f(1,-1));
          return 0;
        }
        int f(int a, int b) {
          if((a>0 &&b >0) || (a<0 && b <0)) {return 7;} else {return 42;}
        }  
    |] [r|
        42
    |]

    it "Boolean operators lazy evaluation" $ \h -> expectProgramSuccess [r|
        int main() {
          printString("&&");
          printBool(test(-1) && test(0));
          printBool(test(-2) && test(1));
          printBool(test(3) && test(-5));
          printBool(test(234234) && test(21321));
          printString("||");
          printBool(test(-1) || test(0));
          printBool(test(-2) || test(1));
          printBool(test(3) || test(-5));
          printBool(test(234234) || test(21321));
          printString("!");
          printBool(true);
          printBool(false);
          return 0 ;
        }
        void printBool(boolean b) {
          if (!b) {
            printString("false");
          } else {
            printString("true");
        }
        return;
        }
        boolean test(int i) {
          printInt(i);
          return i > 0;
        }
    |] [r|
        &&
        -1
        false
        -2
        false
        3
        -5
        false
        234234
        21321
        true
        ||
        -1
        0
        false
        -2
        1
        true
        3
        true
        234234
        true
        !
        true
        false
    |]

    it "Boolean comparison" $ \h -> expectProgramSuccess [r|
        int main() {
          if (true == true) { printInt(42); }
          return 0 ;
        }
    |] [r|
        42
    |]

    it "Test correct boolean lazy evaluation" $ \h -> expectProgramSuccess [r|
        int main() {
          f(1,2);
          return 0;
        }
        void f(int x, int y) {
          if (y > x || e())
            printString("yes");
        }
        boolean e() {
          printString("NOOO");
          return false;
          }
    |] [r|
        yes
    |]

    it "Boolean operators evaluation" $ \h -> expectProgramSuccess [r|
        int main () {
          int x = 4;
          if (3 <= x && 4 != 2 && true) {
            printBool(true);
          } else {
            printString("apa");
          }
          printBool(true == true || dontCallMe(1));
          printBool(4 < -5 && dontCallMe(2));
          printBool(4 == x && true == !false && true);
          printBool(implies(false,false));
          printBool(implies(false,true));
          printBool(implies(true,false));
          printBool(implies(true,true));
          return 0 ;
        }
        boolean dontCallMe(int x) {
          printInt(x);
          return true;
        }
        void printBool(boolean b) {
          if (b) {
            printString("true");
          } else {
            printString("false");
        }
        return;
        }
        boolean implies(boolean x, boolean y) {
          return !x || x == y;
        }
    |] [r|
        true
        true
        false
        true
        true
        true
        false
        true
    |]

    it "Simple boolean lazy evaluation with side-effects" $ \h -> expectProgramSuccess [r|
        int main() {
            printit() && false;
            return 0;
        }
        boolean printit() {
            printString("ahoj");
            return true;
        }
    |] [r|
        ahoj
    |]

    it "Complex nested boolean expression evaluation" $ \h -> expectProgramSuccess [r|
        int main() {
            b(t(1) && f(2));
            b(t(3) && t(4));
            b(t(5) || t(6));
            b(f(7) && t(8));
            b(t(9) && t(10) && t(11));
            b(f(12) || f(13) && t(14));
            return 0;
        }
        boolean f(int a) {
            printInt(a);
            return false;
        }
        boolean t(int a) {
            return !f(a);
        }
        void b(boolean a) {
            if(a)
                printString("true");
            else
                printString("false");
        }
    |] [r|
        1
        2
        false
        3
        4
        true
        5
        true
        7
        false
        9
        10
        11
        true
        12
        13
        false
    |]