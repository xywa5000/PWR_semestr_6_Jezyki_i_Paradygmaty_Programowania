-- Deklaracja pakietu zawierającego funkcje rekurencyjne

with Struct_Lib; use Struct_Lib;

package Recursive_Lib is
   -- Funkcja obliczająca silnię iteracyjnie
   function Factorial_Recursive(N : Integer) return Long_Long_Integer;

   -- Funkcja obliczająca największy wspólny dzielnik (NWD) rekurencyjnie
   function GCD_Recursive(A, B : Natural) return Natural;

   -- Funkcja rozwiązująca równanie diofantyczne rekurencyjnie
   function Diophantine_Recursive(A, B, C : Integer) return Solution;

   -- Funkcja pomocnicza do rozwiązywania równania diofantycznego rekurencyjnie
   function Diophantine_Helper_Recursive(A, B, C : Integer) return Solution;

end Recursive_Lib;
