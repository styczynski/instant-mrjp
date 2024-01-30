{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.PrintUtilsSpec (spec) where

import Test.Utils.Utils
import Text.RawString.QQ
import Test.Hspec

spec :: Spec
spec = do
  describe "Latte generic print utilities" $ do
    it "Print recursive class (default implementation)" $ \h -> expectProgramSuccess [r|
        class Test {
            int f;
            Test[] arr;
            int g;
            Test inner;
        }

        int main() {
            Test t = new Test;
            t.f = 42;
            t.g = 69;
            t.arr = new Test[3];
            Test tt = new Test;
            t.arr[2] = tt;
            Test[] a = new Test[1];
            a[0] = t;
            t.arr[2].arr = a;
            print(a);

            return 0;
        }
    |] [r|
        [Test{inner: null, g: 69, arr: [null, null, Test{inner: null, g: 0, arr: [<recursive>], f: 0}], f: 42}]
    |]

    it "Print recursive class (default implementation)" $ \h -> expectProgramSuccess [r|
        class Test {
            int f;
            Test[] arr;
            int g;
            Test inner;
        }

        class Test2 extends Test {
            string toString() {
                string arrstr = arr.toString();
                string innerstr = "null";
                if (inner != null) {
                    innerstr = inner.toString();
                }
                return "Test2<g="+g+", f="+f+", arr="+arrstr+", inner="+innerstr+">";
            }
        }

        int main() {
            Test t = new Test;
            t.f = 42;
            t.g = 69;
            t.arr = new Test[3];
            Test2 tt = new Test2;
            Test2 tt2 = new Test2;
            tt2.arr = new Test[0];
            t.arr[2] = tt;
            Test[] a = new Test[2];
            a[0] = t;
            a[1] = tt2;
            t.arr[2].arr = a;
            print(a);

            return 0;
        }
    |] [r|
        [Test{inner: null, g: 69, arr: [null, null, Test2<g=0, f=0, arr=[<recursive>, Test2<g=0, f=0, arr=[], inner=null>], inner=null>], f: 42}, Test2<g=0, f=0, arr=[], inner=null>]
    |]
