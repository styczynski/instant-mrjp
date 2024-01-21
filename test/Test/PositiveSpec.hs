{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.PositiveSpec (spec) where

import Test.Utils.Utils
import Text.RawString.QQ
import Test.Hspec

spec :: Spec
spec = do
  describe "Positive examples" $ do
    it "Hello world" $ \h -> expectProgramSuccess [r|
        int main() {
            printString("Hello world");
            return 0;
        }
      |] [r|
        Hello world
      |]
    it "Simple print condition" $ \h -> expectProgramSuccess [r|
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

    it "Correct variable context capture in complex naming collision scenario" $ \h -> expectProgramSuccess [r|
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

    it "Boolean operators" $ \h -> expectProgramSuccess [r|
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

    it "Simple printInt with void call" $ \h -> expectProgramSuccess [r|
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

    it "Testing the return checker" $ \h -> expectProgramSuccess [r|
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
    it "Booleans comparison" $ \h -> expectProgramSuccess [r|
        int main() {
          if (true == true) { printInt(42); }
          return 0 ;
        }
    |] [r|
        42
    |]
    it "Uninitialized int print" $ \h -> expectProgramSuccess [r|
        int main() {
            int x;
            printInt(x);
            return 0;
        }
    |] [r|
        0
    |]

    it "Extended case for negation" $ \h -> expectProgramSuccess [r|
        int main() {
            printInt(- -1);
            int i = 1;
            printInt(-i);
            printInt(2 - -i);
            return 0;
        }
    |] [r|
        1
        -1
        3
    |]


    it "Queue implemenation" $ \h -> expectProgramSuccess [r|
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

    it "Correct variable context capture in unnamed blocks" $ \h -> expectProgramSuccess [r|
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

    it "Empty instructions" $ \h -> expectProgramSuccess [r|
        int main() {
            ;;;;;
            return 0;
        }
    |] [r|

    |]

    it "Fibonnaci implementation" $ \h -> expectProgramSuccess [r|
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

    it "Simple trivial truetful if" $ \h -> expectProgramSuccess [r|
        int main() {
            if (true) {
              printInt(1);
              return 0;
            }
        }
    |] [r|
        1
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

    it "String concatenation in function call" $ \h -> expectProgramSuccess [r|
        int main() {
            printString("a" + "b");
            return 0;
        }
    |] [r|
        ab
    |]

    it "Various factorial implemenations" $ \h -> expectProgramSuccess [r|
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

    it "Condition short-circuiting" $ \h -> expectProgramSuccess [r|
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

    it "Valid virtual calls in complex polymorphic hierarchy" $ \h -> expectProgramSuccess [r|
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

    it "Simple while loop with 2 variables" $ \h -> expectProgramSuccess [r|
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

    it "Bidirectional list implemenatation" $ \h -> expectProgramSuccess [r|
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

    it "Simple calculator implementation" $ \h -> expectProgramSuccess [r|
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

    it "While true with readInt()" $ \h -> expectProgramSuccess [r|
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

    it "Comparison chain" $ \h -> expectProgramSuccess [r|
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

    it "Complex boolean short-circuit case" $ \h -> expectProgramSuccess [r|
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

    it "Printing strings with escape sequences" $ \h -> expectProgramSuccess [r|
        int main() {
            printString("\\a\\n\n\tb\"");
            return 0;
        }
    |] [r|
        \a\n
          b"
    |]

    it "Printing complicated strings with escape sequences" $ \h -> expectProgramSuccess [r|
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

    it "Counter implementation" $ \h -> expectProgramSuccess [r|
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

    it "Input read" $ \h -> expectProgramSuccess [r|
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

    it "Simple while true exit" $ \h -> expectProgramSuccess [r|
        int main() {
            while(true) {
                return 0;
            }
        }
    |] [r|

    |]

    it "Print int -1" $ \h -> expectProgramSuccess [r|
        int main() {
          printInt(-1);
          return 0 ;
        }
    |] [r|
        -1
    |]

    it "Parity checking" $ \h -> expectProgramSuccess [r|
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

    it "Multiple declarations of variables with multiple initializers" $ \h -> expectProgramSuccess [r|
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

    it "Boolean condition overoptimisation" $ \h -> expectProgramSuccess [r|
        int main() {
            print() && false;
            return 0;
        }
        boolean print() {
            printString("ahoj");
            return true;
        }
    |] [r|
        ahoj
    |]

    it "Alternative list implemenatation" $ \h -> expectProgramSuccess [r|
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

    it "Simple integer addition" $ \h -> expectProgramSuccess [r|
        int main() {
            printInt(1 + 1);
            return 0;
        }
    |] [r|
        2
    |]

    it "Arg assignment in function body" $ \h -> expectProgramSuccess [r|
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

    it "BFS implemenatation" $ \h -> expectProgramSuccess [r|
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

    it "Points implemenatation" $ \h -> expectProgramSuccess [r|
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

    it "Empty while block" $ \h -> expectProgramSuccess [r|
        int main() {
            while(false);
            printInt(1);
            return 0;
        }
    |] [r|
        1
    |]

    it "Long repeated sub-calls chain" $ \h -> expectProgramSuccess [r|
        int d() { return 0;}
        int s(int x) {return x + 1;}
        int main() {
          printInt(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(d())))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));  
          return 0;
        }
    |] [r|
        80
    |]

    it "Signed division of negative numbers" $ \h -> expectProgramSuccess [r|
        int main() {
          { printInt(-42 / -1); }
          return 0 ;
        }
    |] [r|
        42
    |]

    it "Simple string print" $ \h -> expectProgramSuccess [r|
        int main() {
            printString("abc");
            return 0;
        }
    |] [r|
        abc
    |]

    it "Alternative fibonacci implemenatation" $ \h -> expectProgramSuccess [r|
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

    it "Simple print of integer by variable" $ \h -> expectProgramSuccess [r|
        int main() {
        int x = 7;
        printInt(x);
        return 0 ;
        }
    |] [r|
        7
    |]

    it "Arithmetic operations" $ \h -> expectProgramSuccess [r|
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

    it "Boolean operations" $ \h -> expectProgramSuccess [r|
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

    it "Merge sort implemenatation" $ \h -> expectProgramSuccess [r|
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

    it "Simple modulo" $ \h -> expectProgramSuccess [r|
        int main() {
            printInt(5 % 3);
            printInt(-5 % 3); // -2 - sic!
            return 0;
        }
    |] [r|
        2
        -2
    |]

    it "String addition" $ \h -> expectProgramSuccess [r|
        int main() {
            string i;
            i + i;
            return 0;
        }
    |] [r|

    |]

    it "Simple const function call" $ \h -> expectProgramSuccess [r|
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

    it "Void return" $ \h -> expectProgramSuccess [r|
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

    it "Parity of positive integers by recursion" $ \h -> expectProgramSuccess [r|
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

    it "Shapes implementation" $ \h -> expectProgramSuccess [r|
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

    it "Complex variable identificator" $ \h -> expectProgramSuccess [r|
        int main() {
            int abcABC000___ = 0;
            return abcABC000___;
        }
    |] [r|

    |]

    it "Alternative linked list implemenatation" $ \h -> expectProgramSuccess [r|
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

    it "Print integer with simple inline expression" $ \h -> expectProgramSuccess [r|
        int main() {
          printInt(2*-2); return 0;
        }
    |] [r|
        -4
    |]

    it "Declaration with massive number of parameters" $ \h -> expectProgramSuccess [r|
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
