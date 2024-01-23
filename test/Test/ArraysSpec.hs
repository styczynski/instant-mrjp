{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.ArraysSpec (spec) where

import Test.Utils.Utils
import Text.RawString.QQ
import Test.Hspec

spec :: Spec
spec = do
  describe "Latte arrays" $ do
    it "Multidimensional arrays and other complex Array scenarios" $ \h -> expectProgramSuccess [r|
        string[][] resize(string[][] matrix, int extra_cells) {
            string[][] matrix2 = new string[][matrix.length + extra_cells];
            int i = 0;
            for (string[] e : matrix) {
                matrix2[i] = e;
                i++;
                printInt(i);
            }
            return matrix2;
        }

        int main() {
            string[] arr = new string[5];
            arr[0] = "Hello";
            printString(arr[0]);
            int[] arr2 = new int[10];
            arr2[0] = 42;
            arr2[3] = 69;
            print(arr2);
            string[][] arr3 = new string[][2];
            arr3[0] = new string[3];
            arr3[0][0] = "Hello!";
            arr3[0][2] = ":D";
            print(arr3);
            print(resize(arr3, 2));
            return 0;  
        }
    |] [r|
        Hello
        [42, 0, 0, 69, 0, 0, 0, 0, 0, 0]
        [[Hello!, null, :D], null]
        1
        2
        [[Hello!, null, :D], null, null, null]
    |]
