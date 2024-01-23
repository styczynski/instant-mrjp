{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.ComplexE2ESpec (spec) where

import Test.Utils.Utils
import Text.RawString.QQ
import Test.Hspec

spec :: Spec
spec = do
  describe "Latte complex fully-featured programs" $ do
    return ()
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
                if (this.isEmpty())
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
                return this.operator(left.value(), right.value());
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
                this.head = newHead;
                this.tail = newTail;
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
            Point3 q = new Point3;
            Point4 r = new Point4;
            q.move(2,4);
            q.moveZ(7);
            Point2 p = q;
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
