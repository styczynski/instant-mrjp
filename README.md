# latte-mrjp
ps386038
Latte compiler (MRJP) 2023/2024

**Stan na 23.01.2024**

## Budowanie u uruchamianie

Program buduje się standardowo przy pomocy polecenia `make` w katalogu głównym projektu:
```bash
    $ make
```

Źródła biblioteki standardowej Latte znajdują się w `src/Runtime/runtime.h` i `src/Runtime/runtime.c`

**Uwaga:**
Projekt wymaga sporej ilości zalezności:
- containers, text, bytestring (Rózne popularne biblioteki)
- array, keys (Rozszerzenia istneijących kontenerów)
- co-log, diagnose (Logowanie i formatowanie błędów)
- mtl, transformers, monad-extras (Dodatki do funktorów i monad)
- lens, tuple-append, generic-lens (Lens i podobne)
- stacked-dag (Ładne formatowanie DAGów do ascii-artów)
- fgl (Konstrukcja grafów)
- fuzzyset (Zbiory wspierające fuzzymatching)
- prettyprinter, prettyprinter-ansi-terminal (Wypisywanie kolorowanych dokumentów)
- time (Obsługa czasu)
- ordered-containers (Implementacja kontenerów pamiętających kolejność)
- deepseq (Wsparcie dla głębokiej ewaluacji struktur; Notatka: w tym momencie większość struktur deklaruje instancje MkData ale nie jest uywana poza scenariuszami debugowania kompilatora)
- directory, process (Standardowe biblioteki System.*)
Powysze zaleności mogą wymagać sporo miejsca a budowanie zajmuje moment za pierwszym razem (niestety są to ograniczenia serwera students)

## Rozwiązanie

Zaimplementowane backendy:
- `Assembler X86_64`

Zaimplementowane funkcjonalności na dzień 23.01.2024:

- Klasy z dziedziczeniem
- Metody wirtualne
- Tablice
- Garbage collection
- Alokacja rejestrów przy pomocy kolorowania grafów
- Lepsze optymializacje peephole
- Globalne i lokalne usuwanie stałych, lepsze usuwanie martwego kodu i lepsza optymalizacja skoków
- Dopuszczane jest uźycie `var` w przypadku deklaracji z wartością np. `var x = foo(2)+"b"`, wtedy typ jest automatycznie inferowany (w innych kontekstach typ `var` nie jest dopusczany)
- Kaźdy obiekt jak w Javie dziedziczy po klasie `Object`
- Typy prymitywne `int`, `bool` i `byte`
- Operacja `+` jest przeciązona tj. jest zdefiniowana jako:
    ```
        (+) :: String -> (int | bool | byte | Object) -> String
        (+) :: str o = str ++ (o.toString)
        (+) :: int -> int -> int
        (+) inta intb = inta + intb
        (+) :: byte -> byte -> byte
        (+) bytea byteb = bytea + byteb
    ```
- Błędy typowania podają kontekst błędu oraz potencjalne sposoby naprawienia go
- Biblioteka standardowa implementująca metody znane z Javy (patrz uwaga 5 w brakach ponizej)
- 112 rónych testów w tym własnych, dostępnych w test/Test/*Spec.hs
- Obsługa generycznej funkcji `print(Object)`. Fajnie działa dla np. `print(int[])`

**Braki w implementacji oraz plany:**
- Poprawione zostały niedociągnięcia z ostatniej wersji rozwiązania, jednakze zliczanie referencji zostało wyłączone poniewaź generowało problemy z dostępem do pamięci

```java
class Object {
    boolean equals(Object other);
    int getHashCode();
    string toString();
}

class String extends Object {
    int charAt(int pos);
    boolean equals(Object other);
    string concat(string other);
    boolean startsWith(string substr);
    boolean endsWith(string substr);
    byte[] getBytes();
    int indexOf(string substr, int startIndex);
    int length();
    string substring(int index, int length);
    string toString();
    int getHashCode();
}

class Array extends Object {
    int length;
    string toString();
}
```
