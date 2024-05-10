-- Deklaracja pakietu zawierającego funkcje iteracyjne

with Struct_Lib; use Struct_Lib;

package Iterative_Lib is
   -- Funkcja obliczająca silnię iteracyjnie
   function Factorial_Iterative(N : Integer) return Long_Long_Integer
      with
         Export         => True,
         Convention     => C,
         External_Name  => "Factorial_Iterative_External_ada";

   -- Funkcja obliczająca największy wspólny dzielnik (NWD) iteracyjnie
   function GCD_Iterative(AA, BB : Natural) return Natural
      with
         Export         => True,
         Convention     => C,
         External_Name  => "GCD_Iterative_External_ada";

   -- Funkcja rozwiązująca równanie diofantyczne iteracyjnie
   function Diophantine_Iterative(AA, BB, CC : Integer) return Solution
      with
         Export         => True,
         Convention     => C,
         External_Name  => "Diophantine_Iterative_External_ada";
   
end Iterative_Lib;
