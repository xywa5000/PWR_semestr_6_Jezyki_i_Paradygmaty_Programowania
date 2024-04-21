-- Testowanie funkcji iteracyjnych

with Iterative_Lib; use Iterative_Lib;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test_Iterative_Lib is
begin
   -- Testowanie funkcji obliczającej silnię
   Put_Line("Factorial of 5: " & Long_Long_Integer'Image(Factorial_Iterative(5)));
   
   -- Testowanie funkcji obliczającej NWD
   Put_Line("GCD of 12 and 18: " & Natural'Image(GCD_Iterative(12, 18)));
   
   -- Testowanie funkcji rozwiązującej równanie diofantyczne
   declare
      Result : Solution := Diophantine_Iterative(10, 6, 14);
   begin
      if Result.Err then
         Put_Line("No solution exists.");
      else
         Put_Line("Solution of 10x + 6y = 14:");
         Put_Line("x = " & Integer'Image(Result.X));
         Put_Line("y = " & Integer'Image(Result.Y));
      end if;
   end;
   
end Test_Iterative_Lib;
