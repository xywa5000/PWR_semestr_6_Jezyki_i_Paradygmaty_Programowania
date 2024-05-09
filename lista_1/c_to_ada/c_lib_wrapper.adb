with Interfaces.C; use Interfaces.C;

package body C_Lib_Wrapper is

    function Factorial_Iterative_External_C (a : int) return Long_Long_Integer
     with
       Import        => True,
       Convention    => C,
       External_Name => "factorial_iterative";

    function GCD_Iterative_External_C (a, b : int) return Natural
     with
       Import        => True,
       Convention    => C,
       External_Name => "gcd_iterative";

    function Diophantine_Iterative_External_C (a, b, c : int) return Solution
     with
       Import        => True,
       Convention    => C,
       External_Name => "diophantine_iterative";

    function Factorial_Recursive_External_C (a : int) return Long_Long_Integer
     with
       Import        => True,
       Convention    => C,
       External_Name => "factorial_recursive";

    function GCD_Recursive_External_C (a, b : int) return Natural
     with
       Import        => True,
       Convention    => C,
       External_Name => "gcd_recursive";

    function Diophantine_Recursive_External_C (a, b, c : int) return Solution
     with
       Import        => True,
       Convention    => C,
       External_Name => "diophantine_recursive";

   -- Implementacja funkcji obliczającej silnię iteracyjnie

   function Factorial_Iterative_C(N : Integer) return Long_Long_Integer is
   begin
      return Long_Long_Integer(Factorial_Iterative_External_C(Interfaces.C.int(N)));
   end Factorial_Iterative_C;

   function GCD_Iterative_C(A, B : Natural) return Natural is
   begin
      return Natural(GCD_Iterative_External_C(Interfaces.C.int(A), Interfaces.C.int(B)));
   end GCD_Iterative_C;

   function Diophantine_Iterative_C(A, B, C : Integer) return Solution is
   begin
      return Diophantine_Iterative_External_C(Interfaces.C.int(A), Interfaces.C.int(B), Interfaces.C.int(C));
   end Diophantine_Iterative_C;

   function Factorial_Recursive_C(N : Integer) return Long_Long_Integer is
   begin
      return Long_Long_Integer(Factorial_Recursive_External_C(Interfaces.C.int(N)));
   end Factorial_Recursive_C;

   function GCD_Recursive_C(A, B : Natural) return Natural is
   begin
      return Natural(GCD_Recursive_External_C(Interfaces.C.int(A), Interfaces.C.int(B)));
   end GCD_Recursive_C;

   function Diophantine_Recursive_C(A, B, C : Integer) return Solution is
   begin
      return Diophantine_Recursive_External_C(Interfaces.C.int(A), Interfaces.C.int(B), Interfaces.C.int(C));
   end Diophantine_Recursive_C;

end C_Lib_Wrapper;