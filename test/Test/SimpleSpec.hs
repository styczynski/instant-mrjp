{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.SimpleSpec (spec) where

import Test.Utils.Utils
import Text.RawString.QQ
import Test.Hspec

spec :: Spec
spec = do
  describe "Latte simple expressions" $ do
    it "Empty void function call" $ \h -> expectProgramSuccess [r|
        int main() {
            p();
            printInt(1);
            return 0;
        }
        void p() {}
    |] [r|
        1
    |]

    it "Declaration of multiple variables of the same type in one statement" $ \h -> expectProgramSuccess [r|
        int main () {
          int x, y;
          x = 45;
          y = -36;
          printInt(x);
          printInt(y);
          return 0 ;
        }
    |] [r|
        45
        -36
    |]

    it "Default initializer for int" $ \h -> expectProgramSuccess [r|
        int main() {
            int x;
            printInt(x);
            return 0;
        }
    |] [r|
        0
    |]

    it "Iteration over 2 variables" $ \h -> expectProgramSuccess [r|
        int main() {
          printInt(fac(5));
          return 0 ;
        }
        int fac (int a) {
          int r;
          int n;
          r = 1;
          n = a;
          while (n > 0)
          {
            r = r * n;
            n = n - 1;
          }
          return r;
        }
    |] [r|
        120
    |]

    it "Input functions" $ \h -> expectProgramSuccessIn ["-37", "foo", "bar"] [r|
        int main() {
          int x = readInt();
          string y = readString();
          string z = readString();
          printInt(x-5);
          printString(y+z);  
          return 0 ;
        }
    |] [r|
        -42
        foobar
    |]

    it "Print -1" $ \h -> expectProgramSuccess [r|
        int main() {
          printInt(-1);
          return 0 ;
        }
    |] [r|
        -1
    |]

    it "Parity of positive integers by loop" $ \h -> expectProgramSuccess [r|
        int main () {
          int y = 17;
          while (y > 0)
            y = y - 2;
          if (y < 0) {
            printInt(0);
            return 0 ;
            }
          else {
            printInt(1);
            return 0 ;
            }
        }
    |] [r|
        0
    |]

    it "Multiple variables of the same type declared" $ \h -> expectProgramSuccess [r|
        int main() {
        int x, y = 7;
        x = -1234234;
        printInt(x);
        printInt(y);
        return 0 ;
        }
    |] [r|
        -1234234
        7
    |]

    it "Negative number divison" $ \h -> expectProgramSuccess [r|
        int main() {
          { printInt(-42 / -1); }
          return 0 ;
        }
    |] [r|
        42
    |]

    it "Declaration and initialization in same statement" $ \h -> expectProgramSuccess [r|
        int main() {
          int x = 7;
          printInt(x);
          return 0 ;
        }
    |] [r|
        7
    |]

    it "Test arithmetic and comparisons" $ \h -> expectProgramSuccess [r|
        int main() {
            int x = 56;
            int y = -23;
            printInt(x+y);
            printInt(x-y);
            printInt(x*y);
            printInt(45/2);
            printInt(78%3);
            printBool(x-y > x+y);
            printBool(x/y <= x*y);
            printString("string"+" "+"concatenation");
            return 0 ;
        }
        void printBool(boolean b) {
          if (b) {
            printString("true");
            return;
          } else {
            printString("false");
            return;
        }
        }
    |] [r|
        33
        79
        -1288
        22
        0
        true
        false
        string concatenation
    |]

    it "Simple printInt with uninitialized integer" $ \h -> expectProgramSuccess [r|
        int main() {
          int x;
          printInt(x);
          return 0;
        }
    |] [r|
        0
    |]

    it "Negative number multiplication in inline expression" $ \h -> expectProgramSuccess [r|
        int main() {
          printInt(2*-2); return 0;
        }
    |] [r|
        -4
    |]

    it "String and int concatenation" $ \h -> expectProgramSuccess [r|
        int main() {
          string x ;
          x = "pi" + 1 ;
          printString(x);
          return 0 ;
        }
    |] [r|
        pi1
    |]

    it "Arithmetic negation of various expressions" $ \h -> expectProgramSuccess [r|
        int main() {
            printInt(-(-1));
            int i = 1;
            printInt(-i);
            printInt(2 - (-i));
            return 0;
        }
    |] [r|
        1
        -1
        3
    |]

    it "Empty expressions as statements" $ \h -> expectProgramSuccess [r|
        int main() {
            ;;;;;
            return 0;
        }
    |] [r|

    |]

    it "Simple inline string concatenation" $ \h -> expectProgramSuccess [r|
        int main() {
            printString("a" + "b");
            return 0;
        }
    |] [r|
        ab
    |]

    it "Simple constant comparisons" $ \h -> expectProgramSuccess [r|
        int main() {
            if(1 <= 1)
                printString("4");
            if(1 >= 1)
                printString("4");
            if(1 > 1)
                printString("5");
            if(1 < 1)
                printString("5");
            if(1 < 2)
                printString("6");
            if(2 > 1)
                printString("6");
            if(1 > 2)
                printString("7");
            if(2 < 1)
                printString("7");
            return 0;
        }
    |] [r|
        4
        4
        6
        6
    |]

    it "Printing string with escape sequences" $ \h -> expectProgramSuccess [r|
        int main() {
            printString("\\a\\n\n\tb\"");
            return 0;
        }
    |] [r|
        \a\n
          b"
    |]

    it "Printing complex string with escape sequences" $ \h -> expectProgramSuccess [r|
        // Autor: Bolek Kulbabinski
        int f(int p){
            int c = p + 2*p;
            printString("\"\npop\npowrot:\ngetstatic java/lang/System/out Ljava/io/PrintStream;\nldc \"zle \"\ninvokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\ngoto powrot\nldc \"");
            return c;
        }
        int main() {
            return f(1) - 3;
        }
    |] [r|
        "
        pop
        powrot:
        getstatic java/lang/System/out Ljava/io/PrintStream;
        ldc "zle "
        invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V
        goto powrot
        ldc "
    |]

    it "Simple inline addition" $ \h -> expectProgramSuccess [r|
        int main() {
            printInt(1 + 1);
            return 0;
        }
    |] [r|
        2
    |]

    it "Simple print of const string" $ \h -> expectProgramSuccess [r|
        int main() {
            printString("abc");
            return 0;
        }
    |] [r|
        abc
    |]

    it "Inline modulo of positive and negative consts" $ \h -> expectProgramSuccess [r|
        int main() {
            printInt(5 % 3);
            printInt(-5 % 3); // This should be 1
            return 0;
        }
    |] [r|
        2
        1
    |]

    it "Inline string addition" $ \h -> expectProgramSuccess [r|
        int main() {
            string i = "";
            i + i;
            return 0;
        }
    |] [r|

    |]

    it "Simple const int print" $ \h -> expectProgramSuccess [r|
        int main() {
            printInt(1);
            return 0;
        }
    |] [r|
        1
    |]

    it "Some strange var indentifier" $ \h -> expectProgramSuccess [r|
        int main() {
            int abcABC000___ = 0;
            return abcABC000___;
        }
    |] [r|

    |]
