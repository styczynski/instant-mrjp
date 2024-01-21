{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.CoreSpec (spec) where

import Test.Utils.Utils
import Text.RawString.QQ
import Test.Hspec

spec :: Spec
spec = do
  describe "Positive examples" $ do
    it "core031" $ \h -> expectProgramSuccess [r|
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

    it "core019" $ \h -> expectProgramSuccess [r|
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

    it "core013" $ \h -> expectProgramSuccess [r|
        /* Test boolean operators. */
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

    it "core020" $ \h -> expectProgramSuccess [r|
        int main() {
            p();
            printInt(1);
            return 0;
        }
        void p() {}
    |] [r|
        1
    |]

    it "core006" $ \h -> expectProgramSuccess [r|
        // Declaration of multiple variables of the same type in one statement:
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

    it "core003" $ \h -> expectProgramSuccess [r|
        // Testing the return checker
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

    it "core004" $ \h -> expectProgramSuccess [r|
        /* allow comparing booleans. */
        int main() {
          if (true == true) { printInt(42); }
          return 0 ;
        }
    |] [r|
        42
    |]

    it "core022" $ \h -> expectProgramSuccess [r|
        int main() {
            int x;
            printInt(x);
            return 0;
        }
    |] [r|
        0
    |]

    it "core014" $ \h -> expectProgramSuccess [r|
        /* Fibonacci. */
        int main () {
          int lo,hi,mx ;
          lo = 1 ;
          hi = lo ;
          mx = 5000000 ;
          printInt(lo) ;
          while (hi < mx) {
            printInt(hi) ;
            hi = lo + hi ;
            lo = hi - lo ;
          }
          return 0 ;
        }
    |] [r|
        1
        1
        2
        3
        5
        8
        13
        21
        34
        55
        89
        144
        233
        377
        610
        987
        1597
        2584
        4181
        6765
        10946
        17711
        28657
        46368
        75025
        121393
        196418
        317811
        514229
        832040
        1346269
        2178309
        3524578
    |]

    it "core021" $ \h -> expectProgramSuccess [r|
        int main() {
            if (true) {
              printInt(1);
              return 0;
            }
        }
    |] [r|
        1
    |]

    it "core001" $ \h -> expectProgramSuccess [r|
        int main() {
          printInt(fac(10));
          printInt(rfac(10));
          printInt(mfac(10));
                printInt(ifac(10));
                string r ; // just to test blocks 
          {
            int n = 10;
            int r = 1;
            while (n>0) {
              r = r * n;
              n--;
            }
            printInt(r);
          }
          printString (repStr("=",60));
          printString ("hello */");
                printString ("/* world") ;
                return 0 ;
        }
        int fac(int a) {
          int r;
          int n;
          r = 1;
          n = a;
          while (n > 0) {
            r = r * n;
            n = n - 1;
          }
          return r;
        }
        int rfac(int n) {
          if (n == 0)
            return 1;
          else
            return n * rfac(n-1);
        }
        int mfac(int n) {
          if (n == 0)
            return 1;
          else
            return n * nfac(n-1);
        }
        int nfac(int n) {
          if (n != 0)
            return mfac(n-1) * n;
          else
            return 1;
        }
        int ifac(int n) { return ifac2f(1,n); }
        int ifac2f(int l, int h) {
                if (l == h)
                  return l;
                if (l > h)
                  return 1;
                int m;
                m = (l + h) / 2;
                return ifac2f(l,m) * ifac2f(m+1,h);
        }
        string repStr(string s, int n) {
          string r = "";
          int i = 0;
          while(i<n) {
            r = r + s;
            i++;
          }
        return r;
        }
    |] [r|
        3628800
        3628800
        3628800
        3628800
        3628800
        ============================================================
        hello */
        /* world
    |]

    it "core024" $ \h -> expectProgramSuccess [r|
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

    it "core010" $ \h -> expectProgramSuccess [r|
        // count function parameters as initialized
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
          return 3;
        }
    |] [r|
        120
    |]

    it "core017" $ \h -> expectProgramSuccess [r|
        /* Test boolean operators */
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

    it "core002" $ \h -> expectProgramSuccess [r|
        /* void expression as statement */
        int main() {
          foo();
          return 0 ;
        }
        void foo() {
          printString("foo");
          return;
        }
    |] [r|
        foo
    |]

    it "core018" $ \h -> expectProgramSuccess [r|
        /* test input */
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

    it "core011" $ \h -> expectProgramSuccess [r|
        /* Test pushing -1. */
        int main() {
          printInt(-1);
          return 0 ;
        }
    |] [r|
        -1
    |]

    it "core016" $ \h -> expectProgramSuccess [r|
        /* parity of positive integers by loop */
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

    it "core008" $ \h -> expectProgramSuccess [r|
        // multiple variables of the same type declared 
        // and possibly initialized in the same statement
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

    it "core027" $ \h -> expectProgramSuccess [r|
        int main() {
          f("bad");
          return 0;
        }
        void f(string arg) {
          arg = "good";
          printString(arg);
        }
    |] [r|
        good
    |]

    it "core026" $ \h -> expectProgramSuccess [r|
        int d() { return 0;}
        int s(int x) {return x + 1;}
        int main() {
          printInt(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(d())))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));  
          return 0;
        }
    |] [r|
        80
    |]

    it "core032" $ \h -> expectProgramSuccess [r|
        /* division */
        int main() {
          { printInt(-42 / -1); }
          return 0 ;
        }
    |] [r|
        42
    |]

    it "core007" $ \h -> expectProgramSuccess [r|
        // declaration and initialization in same statement
        int main() {
        int x = 7;
        printInt(x);
        return 0 ;
        }
    |] [r|
        7
    |]

    it "core012" $ \h -> expectProgramSuccess [r|
        /* Test arithmetic and comparisons. */
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

    it "core028" $ \h -> expectProgramSuccess [r|
        int main() {
          int x;
          printInt(x);
          return 0;
        }
    |] [r|
        0
    |]

    it "core009" $ \h -> expectProgramSuccess [r|
        // Calling functions which take zero parameters
        int main() {
        int x = foo();
        printInt(x);
        return 0 ;
        }
        int foo() {
        return 10;
        }
    |] [r|
        10
    |]

    it "core005" $ \h -> expectProgramSuccess [r|
        /* usage of variable initialized in both branches. */
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

    it "core015" $ \h -> expectProgramSuccess [r|
        /* parity of positive integers by recursion */
        int main () {
          printInt(ev(17)) ;
          return 0 ;
        }
        int ev (int y) {
          if (y > 0)
            return ev (y-2) ;
          else
            if (y < 0)
              return 0 ;
            else
              return 1 ;
        }
    |] [r|
        0
    |]

    it "core025" $ \h -> expectProgramSuccess [r|
        int main() {
          printInt(2*-2); return 0;
        }
    |] [r|
        -4
    |]

    it "core023" $ \h -> expectProgramSuccess [r|
        int main() {
          int a=1,b=2,c=1,d=2,e=1,f=2,g=1,h=2,i=1,j=2,k=1,l=2,m=1,n=2;
          return foo(a,b,c,d,e,f,g,h,i,j,k,l,m,n);
        }
        int foo(int a,int b,int c,int d,int e,int f,int g,
                int h,int i,int j,int k,int l,int m, int n) {
          int r = (2*a+b/2+c+d+e+f+g+h+i+j/2+k+l+m+n)%10;
          printInt(r);
          return r;
        }
    |] [r|
        0
    |]
