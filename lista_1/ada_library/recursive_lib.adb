-- Implementacja pakietu zawierającego funkcje rekurencyjne

with Struct_Lib; use Struct_Lib;

package body Recursive_Lib is

   -- Implementacja funkcji obliczającej silnię rekurencyjnie
   function Factorial_Recursive(N : Integer) return Long_Long_Integer is
   begin
      if N < 0 then
         return -1;
      elsif N = 0 or else N = 1 then
         return 1;
      else
         return Long_Long_Integer(N) * Factorial_Recursive(N - 1);
      end if;
   end Factorial_Recursive;

   -- Implementacja funkcji obliczającej NWD rekurencyjnie
   function GCD_Recursive(A, B : Natural) return Natural is
   begin
      if B = 0 then
         return A;
      else
         return GCD_Recursive(B, A mod B);
      end if;
   end GCD_Recursive;
   
   -- Implementacja funkcji rozwiązującej równanie diofantyczne rekurencyjnie
   function Diophantine_Recursive(A, B, C : Integer) return Solution is
      D : Natural;
   begin
      D := GCD_Recursive(A, B);
      if C mod D /= 0 then
         return (0, 0, True);
      else
         return Diophantine_Helper_Recursive(A, B, C);
      end if;
   end Diophantine_Recursive;

   function Diophantine_Helper_Recursive(A, B, C : Integer) return Solution is
      Sol : Solution;
      Temp : Integer;
   begin
      if B = 0 then
         Sol.X := C / A;
         Sol.Y := 0;
         return Sol;
      else
         Sol := Diophantine_Helper_Recursive(B, A mod B, C);
         Temp := Sol.X;
         Sol.X := Sol.Y;
         Sol.Y := Temp - (A / B) * Sol.Y;
         return Sol;
      end if;
   end Diophantine_Helper_Recursive;

end Recursive_Lib;
