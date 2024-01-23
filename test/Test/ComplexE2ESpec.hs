{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.ComplexE2ESpec (spec) where

import Test.Utils.Utils
import Text.RawString.QQ
import Test.Hspec

spec :: Spec
spec = do
  describe "Latte complex fully-featured programs" $ do
    it "Queue implementation using linked list" $ \h -> expectProgramSuccess [r|
        class Node {
            int elem;
            Node next;
            void setElem (int e)  { elem = e; }
            void setNext (Node n) { next = n; }
            int  getElem () { return elem; }
            Node getNext () { return next; }
        }
        class IntQueue {
            Node front;
            Node rear;
            boolean isEmpty () { return front == (Node)null; }
            void insert (int x) {
                Node last = new Node;
                last.setElem(x);
                if (self.isEmpty())
                front = last;
                else 
                rear.setNext(last);
                rear = last;
            }
            int first () { return front.getElem(); }
            void rmFirst () {
                front = front.getNext();
            }
            int size () {
                Node n = front;
                int res = 0;
                while (n != (Node)null) {
                    n = n.getNext();
                    res++;
                }
                return res;
            }
        }
        int f (int x) {
            return x*x + 3;
        }
        int main () {
            IntQueue q = new IntQueue;
            q.insert(f(3));
            q.insert(5);
            q.insert(7);
            printInt(q.first());
            q.rmFirst();
            printInt(q.size());
            return 0;
        }
    |] [r|
        12
        2
    |]


    it "Bi-directional list implementation using public functions" $ \h -> expectProgramSuccess [r|
        int main(){
            int dlugoscListy = 30;
            listaTest(dlugoscListy );
            return 0;
        }
        void listaTest(int dlugoscListy){
            30;
            Lista lista = zwrocListeDlugosci(dlugoscListy);
            Lista odKonca = przejdzSieNaKoniecIWypisuj(lista, dlugoscListy);
            lista = wrocNaPoczatekIWypisuj(odKonca, dlugoscListy);
            printString("po spacerku wartosc pierwszego elementu:");
            printInt(lista.wartosc);
            if (dlugoscListy >= 5){
                Lista drugi = lista.nastepny;
                printString("po spacerku wartosc drugiego elementu:");
                Lista next = lista.nastepny;
                printInt(next.wartosc);
            } else {
                return;
            }
        }
        Lista przejdzSieNaKoniecIWypisuj(Lista start, int dl){
            Lista a = start, b = start;
            printString("idziemy na koniec listy: ");
            int i = 0;
            while (i < dl){
                printInt(a.wartosc);
                b = a;
                a = a.nastepny;
                i++;
            }
            return b;
        }
        Lista wrocNaPoczatekIWypisuj(Lista odKonca, int dl){
            Lista a = odKonca, b;
            printString("wracamy na poczatek listy: ");
            int w = dl;
            while (w > 0){
                printInt(a.wartosc);
                b = a;
                a = a.poprzedni;
                w--;
            }
            return b;
        }
        class Lista{
            Lista poprzedni;
            Lista nastepny;
            int wartosc;
        }
        Lista zwrocListeDlugosci(int dlugoscListy){
            printString("Krotki test listy:");
            Lista start = new Lista, a, n;
            a = start;
            int wsk = 1;
            start.wartosc = 0;
            while (wsk != dlugoscListy){
                start.poprzedni = null;
                a.nastepny = new Lista;
                n = a;
                a = a.nastepny;
                a.wartosc = wsk;
                a.poprzedni = n;
                wsk++;
            }
            a.nastepny = null;
            printString("wygenerowal liste 2kierunkowa dlugosci :");
            printInt(dlugoscListy);
            printString("__________");
            return start;
        }
    |] [r|
        Krotki test listy:
        wygenerowal liste 2kierunkowa dlugosci :
        30
        __________
        idziemy na koniec listy: 
        0
        1
        2
        3
        4
        5
        6
        7
        8
        9
        10
        11
        12
        13
        14
        15
        16
        17
        18
        19
        20
        21
        22
        23
        24
        25
        26
        27
        28
        29
        wracamy na poczatek listy: 
        29
        28
        27
        26
        25
        24
        23
        22
        21
        20
        19
        18
        17
        16
        15
        14
        13
        12
        11
        10
        9
        8
        7
        6
        5
        4
        3
        2
        1
        0
        po spacerku wartosc pierwszego elementu:
        0
        po spacerku wartosc drugiego elementu:
        1
    |]

    it "Simple Latte calculator implementation" $ \h -> expectProgramSuccess [r|
        int main() {
            Node w = plus(minus(liczba(4), liczba(3)), razy(liczba(2), podziel(liczba(4), liczba(2))));
            printInt(w.value());
            return 0;
        }
        Node plus(Node n1, Node n2) {
            Operator res = new Plus;
            res.left = n1;
            res.right = n2;
            return res;
        }
        Node razy(Node n1, Node n2) {
            Operator res = new Razy;
            res.left = n1;
            res.right = n2;
            return res;
        }
        Node podziel(Node n1, Node n2) {
            Operator res = new Podziel;
            res.left = n1;
            res.right = n2;
            return res;
        }
        Node minus(Node n1, Node n2) {
            Operator res = new Minus;
            res.left = n1;
            res.right = n2;
            return res;
        }
        Node liczba(int l) {
            Liczba res = new Liczba;
            res.v = l;
            return res;
        }
        class Node {
            int value() {
                error();
                return 0;
            }
        }
        class Liczba extends Node {
            int v;
            int value() {
                return v;
            }
        }
        class Operator extends Node {
            Node left;
            Node right;
            int operator(int n1, int n2) {
                error();
                return 0;
            }
            int value() {
                return self.operator(left.value(), right.value());
            }
        }
        class Plus extends Operator {
            int operator(int a, int b) {
                return a + b;
            }
        }
        class Minus extends Operator {
            int operator(int a, int b) {
                return a - b;
            }
        }
        class Razy extends Operator {
            int operator(int a, int b) {
                return a * b;
            }
        }
        class Podziel extends Operator {
            int operator(int a, int b) {
                return a / b;
            }
        }
    |] [r|
        5
    |]


    it "list" $ \h -> expectProgramSuccess [r|
        class list {
            int elem;
            list next;
        }
        int main() {
            printInt(length(fromTo(1,50)));
            printInt(length2(fromTo(1,100)));
            return 0;
        }
        int head (list xs) {
            return xs . elem;
        }
        list cons (int x, list xs) {
            list n;
            n = new list;
            n.elem = x;
            n.next = xs;
            return n;
        }
        int length (list xs) {
            if (xs==(list)null)
                return 0;
            else
                return 1 + length (xs.next);
        }
        list fromTo (int m, int n) {
            if (m>n)
                return (list)null;
            else 
                return cons (m,fromTo (m+1,n));
        }
        int length2 (list xs) {
            int res = 0;
            while (xs != (list)null) {
                res++;
                xs = xs.next;
            }
            return res;
        }
    |] [r|
        50
        100
    |]


    it "BFS implementation using Nodes with references and custom List/Queue" $ \h -> expectProgramSuccess [r|
        class Node{
            boolean visited;
            int value;
            List neighbours;
            void init(int val){
                visited = false;
                value = val;
                neighbours = null;    
            }
            boolean isVisited(){
                return visited;
            }
            void markAsVisited(){
                visited = true;
            }
            int getValue(){
                return value;
            }
            List getNeighbours(){
                return neighbours;
            }
            void addNeighbour(Node n){
                if (neighbours == null){
                    neighbours = new List;
                    neighbours.makeSingleton(n);
                }
                else{
                    List newNeighbours = new List;
                    newNeighbours.cons(n, neighbours);
                    neighbours = newNeighbours;
                }
            }
        }
        class List{
            Node head;
            List tail;
            void makeSingleton(Node node){
                head = node;
                tail = null;
            }
            Node getHead(){
                return head;
            }
            List getTail(){
                return tail;
            }
            void cons(Node newHead, List newTail){
                self.head = newHead;
                self.tail = newTail;
            }
        }
        class Queue{
            List first;
            List last;
            Node get(){
                if (first == null)
                    return null;
                Node retEl = first.head;
                first = first.tail;
                if (first == null)
                    last = null;
                return retEl;
            }
            void put(Node n){
                List newTail = new List;
                newTail.makeSingleton(n);
                if (first == null){
                    first = newTail;
                    last = newTail;
                }
                else{
                    last.cons(last.getHead(), newTail);
                    last = newTail;
                }
            }
            boolean isEmpty(){
                return (first == null);
            }
        }
        int main(){
            Node graph = prepareData();
            graph.markAsVisited();
            Queue q = new Queue;
            q.put(graph);
            bfs(q);
            return 0;
        }
        Node prepareData(){
            Node n1 = new Node;
            n1.init(1);
            Node n2 = new Node;
            n2.init(2);
            Node n3 = new Node;
            n3.init(3);
            Node n4 = new Node;
            n4.init(4);
            Node n5 = new Node;
            n5.init(5);
            Node n6 = new Node;
            n6.init(6);
            Node n7 = new Node;
            n7.init(7);
            Node n8 = new Node;
            n8.init(8);
            Node n9 = new Node;
            n9.init(9);
            n1.addNeighbour(n3);
            n1.addNeighbour(n2);
            n2.addNeighbour(n3);
            n3.addNeighbour(n6);
            n3.addNeighbour(n5);
            n3.addNeighbour(n4);
            n4.addNeighbour(n2);
            n5.addNeighbour(n7);
            n7.addNeighbour(n8);
            n8.addNeighbour(n9);
            n9.addNeighbour(n5);
            return n1;
        }
        void bfs(Queue q){
            while (! q.isEmpty()){
                Node el = q.get();
                printInt(el.getValue());
                List neigh = el.getNeighbours();
                while(neigh != null){
                    Node n = neigh.getHead();
                    if (!n.isVisited()){
                        n.markAsVisited();
                        q.put(n);
                    }
                    neigh = neigh.getTail();
                }
            }
        }
    |] [r|
        1
        2
        3
        4
        5
        6
        7
        8
        9
    |]

    it "Geometric points utilities" $ \h -> expectProgramSuccess [r|
        class Point2 {
            int x;
            int y;
            void move (int dx, int dy) {
                x = x + dx;
                y = y + dy;
            }
            int getX () { return x; }
            int getY () { return y; }
        }
        class Point3 extends Point2 {
            int z;
            void moveZ (int dz) {
                z = z + dz;
            }
            int getZ () { return z; }
        }
        class Point4 extends Point3 {
            int w;
            void moveW (int dw) {
                w = w + dw;
            }
            int getW () { return w; }
        }
        int main () {
            Point2 p = new Point3;
            Point3 q = new Point3;
            Point4 r = new Point4;
            q.move(2,4);
            q.moveZ(7);
            p = q;
            p.move(3,5);
            r.move(1,3);
            r.moveZ(6);
            r.moveW(2);
            printInt(p.getX());  
            printInt(p.getY());  
            printInt(q.getZ());  
            printInt(r.getW());
            return 0;
        }
    |] [r|
        5
        9
        7
        2
    |]


    it "Iterative Fibonacci implementation" $ \h -> expectProgramSuccessIn ["23"] [r|
        int fibonacci(int n) {
            if (n <= 1) {
                return n;
            }
            int fib_a = 0;
            int fib_b = 1;
            int tmp;
            int i = 2;
            while (i <= n) {
                tmp = fib_b + fib_a;
                fib_a = fib_b;
                fib_b = tmp;
                i++;
            }
            return fib_b;
        }
        int main() {
            int i = readInt();
            if (i >= 0) {
                printInt(fibonacci(i));
                return 0;
            } else {
                printString("Expected a non-negative integer, but got:");
                printInt(i);
                return 1;
            }
        }
    |] [r|
        28657
    |]

    it "Mergesort implemenatation with public functions and custom List class" $ \h -> expectProgramSuccess [r|
        int main(){
            int dlugoscListy = 30;
            testMergeSort(dlugoscListy);
            return 0;
        }
        void testMergeSort(int dlugoscListy){
            Lista start = generujTablicoListeDoSortowaniaMerge13co7Malejaco(dlugoscListy, true);
            Lista malejaco = mergeSort(start, 0, dlugoscListy);
            przejdzSieNaKoniecIWypisuj(malejaco, dlugoscListy);
        }
        Lista mergeSort(Lista start, int pocz, int kon1Za){
            Lista i1, i2;
            if (kon1Za - pocz > 1){
                int srodek = (kon1Za - pocz) /2 + pocz;
                i1 = mergeSort(start, pocz, srodek);
                i2 = mergeSort(start, srodek, kon1Za);
                return scalaj(i1, srodek - pocz, i2, kon1Za - srodek);
            }
            Lista n = new Lista;
            n.wartosc = pokazWartosc(start, pocz);
            return n;
        }
        Lista scalaj(Lista lj, int ljLength, Lista ld, int ldLength){
            int w1 = 0, w2 = 0, ws = 0;
            Lista start = generujTablicoListeDoSortowaniaMerge13co7Malejaco(ljLength + ldLength, false);
            int wart = pokazWartosc(lj, w1);
            int wart2 = pokazWartosc(ld, w2);
            while (ws < ljLength + ldLength){
                if (w1 == ljLength)	wart = -1;
                else wart = pokazWartosc(lj, w1);
                if (w2 == ldLength)	wart2 = -1;
                else wart2 = pokazWartosc(ld, w2);
                if (wart2 > wart){
                    w2++;		
                    ladujWartosc(start, ws, wart2);
                } else{
                    w1++;
                    ladujWartosc(start, ws, wart);
                }
                ws++;
            }
            return start;
        }
        int pokazWartosc(Lista start, int poz){
            int w = 0;
            Lista a = start;
            while (w != poz) {
                a = a.nastepny;
                w++;
            }
            return a.wartosc;
        }
        void ladujWartosc(Lista start, int poz, int wartosc){
            int w = 0;
            Lista a = start;
            while (w != poz) {
                a = a.nastepny;
                w++;
            }
            a.wartosc = wartosc;
        }
        Lista generujTablicoListeDoSortowaniaMerge13co7Malejaco(int dlugoscListy, boolean pisz){
            if (pisz) printString("robimy liste do mergeSorta:");
            Lista start = new Lista, a, n;
            a = start;
            int wsk = 1;
            start.wartosc = 0;
            if (pisz) printInt(start.wartosc);
            while (wsk != dlugoscListy){
                start.poprzedni = null;
                a.nastepny = new Lista;
                n = a;
                a = a.nastepny;
                if (wsk % 5 == 3) a.wartosc = dlugoscListy - wsk / 2;
                else a.wartosc = wsk % 13;
                a.poprzedni = n;
                //a.wartosc = wsk;
                if (pisz) printInt(a.wartosc);
                wsk++;
            }
            a.nastepny = null;
            if (pisz) printString("wygenerowal liste 2kierunkowa dziwna dlugosci :");
            if (pisz) printInt(dlugoscListy);
            if (pisz) printString("__________");
            return start;
        }
        Lista przejdzSieNaKoniecIWypisuj(Lista start, int dl){
            Lista a = start, b = start;
            printString("idziemy na koniec listy (ma byc nierosnaco): ");
            int wsk = 0;
            while (wsk < dl){
                printInt(a.wartosc);
                b = a;
                a = a.nastepny;
                wsk++;
            }
            return b;
        }
        class Lista{
            Lista poprzedni;
            Lista nastepny;
            int wartosc;
        }
    |] [r|
        robimy liste do mergeSorta:
        0
        1
        2
        29
        4
        5
        6
        7
        26
        9
        10
        11
        12
        24
        1
        2
        3
        4
        21
        6
        7
        8
        9
        19
        11
        12
        0
        1
        16
        3
        wygenerowal liste 2kierunkowa dziwna dlugosci :
        30
        __________
        idziemy na koniec listy (ma byc nierosnaco): 
        29
        26
        24
        21
        19
        16
        12
        12
        11
        11
        10
        9
        9
        8
        7
        7
        6
        6
        5
        4
        4
        3
        3
        2
        2
        1
        1
        1
        0
        0
    |]


    it "Geometric shapes utilities" $ \h -> expectProgramSuccess [r|
        class Node {
            Shape elem;
            Node next;
            void setElem(Shape c) { elem = c; }
            void setNext(Node n) { next = n; }
            Shape getElem() { return elem; }
            Node getNext() { return next; }
        }
        class Stack {
            Node head;
            void push(Shape c) {
                Node newHead = new Node;
                newHead.setElem(c);
                newHead.setNext(head);
                head = newHead;
            }
            boolean isEmpty() {
                return head==(Node)null;
            }
            Shape top() {
                return head.getElem();
            }
            void pop() {
                head = head.getNext();
            }
        }
        class Shape {
            void tell () {
                printString("I'm a shape");
            }
            void tellAgain() {
                printString("I'm just a shape");
            }
        }
        class Rectangle extends Shape {
            void tellAgain() {
                printString("I'm really a rectangle");
            }
        }
        class Circle extends Shape {
            void tellAgain() {
                printString("I'm really a circle");
            }
        }
        class Square extends Rectangle {
            void tellAgain() {
                printString("I'm really a square");
            }
        }
        int main() {
            Stack stk = new Stack;
            Shape s = new Shape;
            stk.push(s);
            s = new Rectangle;
            stk.push(s);
            s = new Square;
            stk.push(s);
            s = new Circle;
            stk.push(s);
            while (!stk.isEmpty()) {
                s = stk.top();
                s.tell();
                s.tellAgain();
                stk.pop();
            }
            return 0;
        }
    |] [r|
        I'm a shape
        I'm really a circle
        I'm a shape
        I'm really a square
        I'm a shape
        I'm really a rectangle
        I'm a shape
        I'm just a shape
    |]

    it "ALternative linked-list implemenatation" $ \h -> expectProgramSuccess [r|
        class Node {
            int elem;
            Node next;
            void setElem(int c) { elem = c; }
            void setNext(Node n) { next = n; }
            int getElem() { return elem; }
            Node getNext() { return next; }
        }
        class Stack {
            Node head;
            void push(int c) {
                Node newHead = new Node;
                newHead.setElem(c);
                newHead.setNext(head);
                head = newHead;
            }
            boolean isEmpty() {
                return head==(Node)null;
            }
            int top() {
                return head.getElem();
            }
            void pop() {
                head = head.getNext();
            }
        }
        int main() {
            Stack s = new Stack;
            int i= 0;
            while (i<10) {
                s.push(i);
                i++;
            }
            while (!s.isEmpty()) {
                printInt(s.top());
                s.pop();
            }
            return 0;
        }
    |] [r|
        9
        8
        7
        6
        5
        4
        3
        2
        1
        0
    |]
