# latte-mrjp
ps386038
Latte compiler (MRJP) 2023/2024

**Stan na 08.01.2024**

## Budowanie u uruchamianie

Program buduje się standardowo przy pomocy polecenia `make` w katalogu głównym projektu:
```bash
    $ make
```

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

Zaimplementowane funkcjonalności na dzień 08.01.2024:

- Klasy z dziedziczeniem (patrz uwaga 1 w brakach ponizej)
- Metody wirtualne (patrz uwaga 1 w brakach ponizej)
- Tablice (patrz uwaga 1 w brakach ponizej)
- Odśmiecanie (patrz uwaga 1 w brakach ponizej)
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
- Błędy typowania podają kontekst błędu oraz potencjalne sposoby naprawienia go (patrz uwaga 2 w brakach ponizej)
- Biblioteka standardowa implementująca metody znane z Javy (patrz uwaga 5 w brakach ponizej)

**Braki w implementacji oraz plany:**
1. Poprawka: Z powodu złego liczenia offestów na polach (jest to nietrywialny błąd, nad którym muszę się zastanowić) uruchamianie kodu z klasami powoduje błędy naruszenia pamięci
2. Poprawka: Wiadomości diagnostyczne nie są az tak dobre jak bym obie tego zyczył i bedą dalej ulepszane, ale zachęcam do sprawdzenia np. cykli z klasami
3. Nowa funkcjonalność: Zamierzam dodać implementację uliniawiania funkcji (inlining) - Jeszcze nie rozpoczęta
4. Nowa funkcjonalność: Zamierzam dodać zmienne indukcyjne i redukcję mocy - Jeszcze nie rozpoczęta
5. Poprawka: Aktualnie biblioteka standardowa oferuje kilka klas wbudowanych, zamierzam rozszerzyć ten reperuar o (moze) podstawowe kontenery znane z Javy. Aktualnie następujące metody są wspierane:
6. Poprawka: Dla duzej ilosci argumentow funkcje źle alokują rejestry

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
