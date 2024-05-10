-- Deklaracja pakietu zawierającego funkcje rekurencyjne

with Struct_Lib; use Struct_Lib;

package Recursive_Lib is
   -- Funkcja obliczająca silnię iteracyjnie
   function Factorial_Recursive(N : Integer) return Long_Long_Integer
      with
         Export         => True,
         Convention     => C,
         External_Name  => "Factorial_Recursive_External_ada";

   -- Funkcja obliczająca największy wspólny dzielnik (NWD) rekurencyjnie
   function GCD_Recursive(A, B : Natural) return Natural
      with
         Export         => True,
         Convention     => C,
         External_Name  => "GCD_Recursive_External_ada";

   -- Funkcja rozwiązująca równanie diofantyczne rekurencyjnie
   function Diophantine_Recursive(A, B, C : Integer) return Solution
      with
         Export         => True,
         Convention     => C,
         External_Name  => "Diophantine_Recursive_External_ada";

   -- Funkcja pomocnicza do rozwiązywania równania diofantycznego rekurencyjnie
   function Diophantine_Helper_Recursive(A, B, C : Integer) return Solution
      with
         Export         => True,
         Convention     => C,
         External_Name  => "Diophantine_Helper_Recursive_External_ada";

end Recursive_Lib;
