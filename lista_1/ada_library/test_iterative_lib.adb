-- Testowanie funkcji iteracyjnych

with Iterative_Lib; use Iterative_Lib;
with Struct_Lib; use Struct_Lib;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test_Iterative_Lib is
   N, A, B, C : Integer;
   N_Str, A_Str, B_Str, C_Str : String(1..100); -- Zmienne na wartości wprowadzone z klawiatury
   N_Length, A_Length, B_Length, C_Length : Natural; -- Zmienne na długość wprowadzonych ciągów
   Result : Solution;
begin
   -- Testowanie funkcji obliczającej silnię
   Put_Line("Podaj liczbę naturalną do obliczenia silni: ");
   Get_Line(N_Str, N_Length); 
   
   -- Konwersja na Integer
   N := Integer'Value(N_Str(1 .. N_Length));
   
   Put_Line("Factorial of " & Integer'Image(N) & ": " & Long_Long_Integer'Image(Factorial_Iterative(N)));
   
   -- Testowanie funkcji obliczającej NWD
   Put_Line("Podaj dwie liczby naturalne do obliczenia NWD: ");
   Get_Line(A_Str, A_Length); 
   Get_Line(B_Str, B_Length); 
   
   -- Konwersja na Integer
   A := Integer'Value(A_Str(1 .. A_Length));
   B := Integer'Value(B_Str(1 .. B_Length));
   
   Put_Line("GCD of " & Natural'Image(A) & " and " & Natural'Image(B) & ": " & Natural'Image(GCD_Iterative(A, B)));
   
   -- Testowanie funkcji rozwiązującej równanie diofantyczne
   Put_Line("Podaj współczynniki a, b i c dla równania diofantycznego ax + by = c: ");
   Get_Line(A_Str, A_Length); 
   Get_Line(B_Str, B_Length); 
   Get_Line(C_Str, C_Length); 
   
   -- Konwersja na Integer
   A := Integer'Value(A_Str(1 .. A_Length));
   B := Integer'Value(B_Str(1 .. B_Length));
   C := Integer'Value(C_Str(1 .. C_Length));
   
   Result := Diophantine_Iterative(A, B, C);
   
   if Result.Err then
      Put_Line("No solution exists.");
   else
      Put_Line("Solution of " & Integer'Image(A) & "x + " & Integer'Image(B) & "y = " & Integer'Image(C) & ":");
      Put_Line("x = " & Integer'Image(Result.X));
      Put_Line("y = " & Integer'Image(Result.Y));
   end if;
end Test_Iterative_Lib;
