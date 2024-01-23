{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.ClassesSpec (spec) where

import Test.Utils.Utils
import Text.RawString.QQ
import Test.Hspec

spec :: Spec
spec = do
  describe "Latte classes" $ do
    it "Complex class hierarchy" $ \h -> expectProgramSuccess [r|
        class A {
            void print() {
                printString("A");
            }
        }
        class B extends A {
            void print() {
                printString("B");
            }
        }
        class C extends B {
            void print() {
                printString("C");
            }
        }
        class D extends C {
            void print() {
                printString("D");
            }
        }
        B fun(){
            // Return z podtypem.
            return new C;
        }
        A fun2(B param){
            return param;
        }
        int main(){
            // Inicjalizacja podtypem.
            A z1 = new B;
            z1.print();
            A z2 = fun();
            z2.print();
            // Przypisanie podtypu.
            z2 = new B;
            z2.print();
            // Przekazanie podtypu jako parametr.
            A z3 = fun2(new C);
            z3.print();
            // Porownywanie z podtypem.
            B t1 = new B;
            A t2 = t1;
            if (t1 == t2) {
                printString("tak");
            }
            if (t1 != t2) {
                printString("nie");
            }
            return 0;
        }
    |] [r|
        B
        C
        B
        C
        tak
    |]

    it "Simple counter class" $ \h -> expectProgramSuccess [r|
        int main () {
            Counter c;
            c = new Counter;
            c.incr();
            c.incr();
            c.incr();
            int x = c.value();
            printInt(x);
            return 0;
        }
        class Counter {
            int val;
            void incr () {val++; return;}
            int value () {return val;}
        }
    |] [r|
        3
    |]

    it "Testing String methods" $ \h -> expectProgramSuccess [r|
        void foo(string s) {
            string ss = "Hello " + s + "!";
            printString(ss);
            printInt(ss.length());
            printString(ss.substring(2, 3));
            printBoolean(ss.endsWith("!"));
            printBoolean(ss.endsWith("?"));
            printBoolean(ss.startsWith("h"));
            printBoolean(ss.startsWith("H"));
            printBoolean(ss.equals("Hello Mark!"));
            printBoolean(ss.equals("Hello John!"));
            printBoolean(ss.equals("Hello John"));
            printInt(ss.charAt(0));
            printInt(ss.getHashCode());
            printInt(("Hello John!").getHashCode());
            printInt(("Hello John?").getHashCode());
        }

        int main() {
            foo("John");
            return 0;  
        }
    |] [r|
        Hello John!
        11
        llo
        true
        false
        false
        true
        false
        true
        false
        72
        2101530907
        2101530907
        1799533765
    |]