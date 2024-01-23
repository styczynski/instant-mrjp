{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.FunctionsSpec (spec) where

import Test.Utils.Utils
import Text.RawString.QQ
import Test.Hspec

spec :: Spec
spec = do
  describe "Latte functions calls" $ do

    it "Function with large number of parameters" $ \h -> expectProgramSuccess [r|
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

    it "Recursive function calls" $ \h -> expectProgramSuccess [r|
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

    it "Calling functions which take zero parameters" $ \h -> expectProgramSuccess [r|
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

    it "Deep linear nested unary function calls" $ \h -> expectProgramSuccess [r|
        int d() { return 0;}
        int s(int x) {return x + 1;}
        int main() {
          printInt(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(d())))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));  
          return 0;
        }
    |] [r|
        80
    |]

    it "Function parameter assignment" $ \h -> expectProgramSuccess [r|
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

    it "Void expression as statement" $ \h -> expectProgramSuccess [r|
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

    it "Iterative Fibonacci implementation" $ \h -> expectProgramSuccess [r|
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

    it "Multiple factorial implementations" $ \h -> expectProgramSuccess [r|
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

    it "Simple void statement" $ \h -> expectProgramSuccess [r|
        int main() {
            run();
            return 0;
        }
        void run() {
            printInt(0);
            if(true)
                return;
            printInt(1);
        }
    |] [r|
        0
    |]