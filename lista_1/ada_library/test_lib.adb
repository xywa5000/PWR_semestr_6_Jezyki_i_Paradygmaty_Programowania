with Ada.Text_IO; use Ada.Text_IO;
with Iterative_Lib; use Iterative_Lib;
with Recursive_Lib; use Recursive_Lib;
with Struct_Lib; use Struct_Lib;

procedure Test_Lib is

   procedure Test_Factorial(Input : Integer; Answer : Long_Long_Integer) is
      Result_Iterative, Result_Recursive : Long_Long_Integer;
   begin
      Result_Iterative := Factorial_Iterative(Input);
      Result_Recursive := Factorial_Recursive(Input);

      Put_Line("    iterative : " & Integer'Image(Input) & "! = " & Long_Long_Integer'Image(Result_Iterative) &
               " : " & (if Result_Iterative = Answer then "TRUE" else "FALSE"));
      Put_Line("    recursive : " & Integer'Image(Input) & "! = " & Long_Long_Integer'Image(Result_Recursive) &
               " : " & (if Result_Recursive = Answer then "TRUE" else "FALSE"));
   end Test_Factorial;

   procedure Test_GCD(Input_X, Input_Y : Positive; Answer : Positive) is
      Result_Iterative, Result_Recursive : Positive;
   begin
      Result_Iterative := GCD_Iterative(Input_X, Input_Y);
      Result_Recursive := GCD_Recursive(Input_X, Input_Y);

      Put_Line("    iterative : gcd(" & Positive'Image(Input_X) & ", " & Positive'Image(Input_Y) & ") = " &
               Positive'Image(Result_Iterative) & " : " & (if Result_Iterative = Answer then "TRUE" else "FALSE"));
      Put_Line("    recursive : gcd(" & Positive'Image(Input_X) & ", " & Positive'Image(Input_Y) & ") = " &
               Positive'Image(Result_Recursive) & " : " & (if Result_Recursive = Answer then "TRUE" else "FALSE"));
   end Test_GCD;

   procedure Test_Diophantine(Input_A, Input_B, Input_C : Integer) is
      Sol_Iterative, Sol_Recursive : Solution;
      Iterative_Condition, Recursive_Condition : Boolean;
   begin
      Sol_Iterative := Diophantine_Iterative(Input_A, Input_B, Input_C);
      Sol_Recursive := Diophantine_Recursive(Input_A, Input_B, Input_C);

      Iterative_Condition :=
         (Sol_Iterative.Err and then Input_C mod GCD_Iterative(Input_A, Input_B) /= 0) or else
         ((Input_A * Sol_Iterative.X) + (Input_B * Sol_Iterative.Y) = Input_C);

      Recursive_Condition :=
         (Sol_Recursive.Err and then Input_C mod GCD_Recursive(Input_A, Input_B) /= 0) or else
         ((Input_A * Sol_Recursive.X) + (Input_B * Sol_Recursive.Y) = Input_C);

      Put_Line("    iterative : diophantine(" & Integer'Image(Input_A) & "x + " &
               Integer'Image(Input_B) & "y = " & Integer'Image(Input_C) & ") = " &
               Integer'Image(Sol_Iterative.X) & ", " & Integer'Image(Sol_Iterative.Y) &
               " : err = " & Boolean'Image(Sol_Iterative.Err) & " : " &
               (if Iterative_Condition then "TRUE" else "FALSE"));

      Put_Line("    recursive : diophantine(" & Integer'Image(Input_A) & "x + " &
               Integer'Image(Input_B) & "y = " & Integer'Image(Input_C) & ") = " &
               Integer'Image(Sol_Recursive.X) & ", " & Integer'Image(Sol_Recursive.Y) &
               " : err = " & Boolean'Image(Sol_Recursive.Err) & " : " &
               (if Recursive_Condition then "TRUE" else "FALSE"));
   end Test_Diophantine;

begin

   Put_Line("Predefined tests:" & ASCII.LF);

   Put_Line("  Factorial:");
   Test_Factorial(0, 1);
   Test_Factorial(1, 1);
   Test_Factorial(4, 24);
   Test_Factorial(5, 120);
   Test_Factorial(14, 87178291200);

   Put_Line("  GCD:");
   Test_GCD(1, 20, 1);
   Test_GCD(5, 25, 5);
   Test_GCD(14, 7, 7);
   Test_GCD(123456, 789, 3);
   Test_GCD(203167, 296703, 37);

   Put_Line("  Diophantine:");
   Test_Diophantine(3, 5, 6);
   Test_Diophantine(8, 6, 11);
   Test_Diophantine(68, 143, 45);
   Test_Diophantine(21, 209, 13);
   Test_Diophantine(31, 747, 90);

end Test_Lib;
