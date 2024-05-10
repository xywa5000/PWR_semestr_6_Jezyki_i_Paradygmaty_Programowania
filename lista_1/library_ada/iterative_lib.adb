-- Implementacja pakietu zawierającego funkcje iteracyjne

with Struct_Lib; use Struct_Lib;

package body Iterative_Lib is

   -- Implementacja funkcji obliczającej silnię iteracyjnie
   function Factorial_Iterative(N : Integer) return Long_Long_Integer is
      Result : Long_Long_Integer := 1;
   begin
      if N < 0 then
         return -1;
      end if;
      for I in 2 .. N loop
         Result := Result * Long_Long_Integer(I);
      end loop;
      return Result;
   end Factorial_Iterative;

   -- Implementacja funkcji obliczającej NWD iteracyjnie
   function GCD_Iterative(AA, BB : Natural) return Natural is
      Temp : Natural;
      A : Natural;
      B : Natural;
   begin
      A := AA;
      B := BB;
      while B /= 0 loop
         Temp := B;
         B := A mod B;
         A := Temp;
      end loop;
      return A;
   end GCD_Iterative;

   -- Implementacja funkcji rozwiązującej równanie diofantyczne iteracyjnie
   function Diophantine_Iterative(AA, BB, CC : Integer) return Solution is
      GCD_AB : Integer := GCD_Iterative(AA, BB);
      X : Integer := 1;
      Y : Integer := 0;
      A : Integer := AA;
      B : Integer := BB;
      C : Integer := CC;
      X1 : Integer := 0;
      Y1 : Integer := 1;
      Temp : Integer;
      K : Integer;
      Result : Solution;
   begin
      if C mod GCD_AB /= 0 then
         Result.Err := True;
         return Result;
      end if;
      
      while B /= 0 loop
         declare
            Q : Integer := A / B;
            R : Integer := A mod B;
         begin
            Temp := X1;
            X1 := X - Q * X1;
            X := Temp;
            
            Temp := Y1;
            Y1 := Y - Q * Y1;
            Y := Temp;
            
            A := B;
            B := R;
         end;
      end loop;
      
      K := C / GCD_Iterative(A, B);
      Result.X := X * K;
      Result.Y := Y * K;
      
      return Result;
   end Diophantine_Iterative;

end Iterative_Lib;
