-- Deklaracja pakietu zawierającego wrappery do bliblioteki w c

package C_Lib_Wrapper is

    type Solution is record
      X : Integer;
      Y : Integer;
      Err : Boolean := False;
    end record;

    -- Funkcja obliczająca silnię iteracyjnie
    function Factorial_Iterative_C(N : Integer) return Long_Long_Integer;

    -- Funkcja obliczająca największy wspólny dzielnik (NWD) iteracyjnie
    function GCD_Iterative_C(A, B : Natural) return Natural;


    -- Funkcja rozwiązująca równanie diofantyczne iteracyjnie
    function Diophantine_Iterative_C(A, B, C : Integer) return Solution;

    function Factorial_Recursive_C(N : Integer) return Long_Long_Integer;


    -- Funkcja obliczająca największy wspólny dzielnik (NWD) rekurencyjnie
    function GCD_Recursive_C(A, B : Natural) return Natural;


    -- Funkcja rozwiązująca równanie diofantyczne rekurencyjnie
    function Diophantine_Recursive_C(A, B, C : Integer) return Solution;

end C_Lib_Wrapper;