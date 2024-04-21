-- Deklaracja pakietu zawierającego funkcje iteracyjne

package Iterative_Lib is
   -- Funkcja obliczająca silnię iteracyjnie
   function Factorial_Iterative(N : Integer) return Long_Long_Integer;

   -- Funkcja obliczająca największy wspólny dzielnik (NWD) iteracyjnie
   function GCD_Iterative(AA, BB : Natural) return Natural;

   -- Typ rozwiązania równania diofantycznego
   type Solution is record
      X : Integer;
      Y : Integer;
      Err : Boolean := False;
   end record;

   -- Funkcja rozwiązująca równanie diofantyczne iteracyjnie
   function Diophantine_Iterative(AA, BB, CC : Integer) return Solution;
   
end Iterative_Lib;
