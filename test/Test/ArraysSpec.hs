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

    it "Simple array length" $ \h -> expectProgramSuccess [r|
        int main() {
            printInt((new int[1]).length);
            return 0;
        }
    |] [r|
        1
    |]

    it "Simple array new (string)" $ \h -> expectProgramSuccess [r|
        int main() {
            string[] a = new string[1];
            return 0;
        }
    |] [r|

    |]

    it "Accessing inline new array" $ \h -> expectProgramSuccess [r|
        int main() {
            (new int[1])[0];
            return 0;
        }
    |] [r|

    |]

    it "Accessing array of strings" $ \h -> expectProgramSuccess [r|
        int main() {
            string[] a = new string[1];
            a[0] = "abc";
            printString(a[0]);
            return 0;
        }
    |] [r|
        abc
    |]

    it "Array Heapsort" $ \h -> expectProgramSuccessIn ["8", "-5", "0", "1000000", "29", "14", "-1", "10", "-3"] [r|
        void heapDown(int[] heap, int index, int heapSize) {
            while (index * 2 < heapSize - 1) {
                int left = index * 2 + 1;
                int right = left + 1;
                int max = left;
                if (right < heapSize && heap[right] > heap[max]) {
                    max = right;
                }
                if (heap[max] > heap[index]) {
                    int temp = heap[max];
                    heap[max] = heap[index];
                    heap[index] = temp;
                    index = max;
                } else {
                    return;
                }
            }
        }
        int extractMax(int[] heap, int heapSize) {
            int max = heap[0];
            heap[0] = heap[heapSize - 1];
            heapDown(heap, 0, heapSize - 1);
            return max;
        }
        void heapSort(int[] heap) {
            int i = heap.length / 2;
            while (i >= 0) {
                heapDown(heap, i, heap.length);
                i--;
            }
            i = heap.length - 1;
            while (i >= 0) {
                heap[i] = extractMax(heap, i + 1);
                i--;
            }
        }
        int main() {
            int n = readInt();
            int[] tab = new int[n];
            int i = 0;
            while (i < n) {
                tab[i] = readInt();
                i++;
            }
            heapSort(tab);
            for (int elem : tab) {
                printInt(elem);
            }
            return 0;
        }
    |] [r|
        -5
        -3
        -1
        0
        10
        14
        29
        1000000
    |]