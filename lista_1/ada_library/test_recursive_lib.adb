-- Testowanie funkcji iteracyjnych

with Recursive_Lib; use Recursive_Lib;
with Struct_Lib; use Struct_Lib;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test_Recursive_Lib is
begin
   -- Testowanie funkcji obliczającej silnię
   Put_Line("Factorial of 5: " & Long_Long_Integer'Image(Factorial_Recursive(5)));
   
   -- Testowanie funkcji obliczającej NWD
   Put_Line("GCD of 12 and 18: " & Natural'Image(GCD_Recursive(12, 18)));
   
   -- Testowanie funkcji rozwiązującej równanie diofantyczne
   declare
      Result : Solution := Diophantine_Recursive(10, 6, 14);
   begin
      if Result.Err then
         Put_Line("No solution exists.");
      else
         Put_Line("Solution of 10x + 6y = 14:");
         Put_Line("x = " & Integer'Image(Result.X));
         Put_Line("y = " & Integer'Image(Result.Y));
      end if;
   end;
   
end Test_Recursive_Lib;