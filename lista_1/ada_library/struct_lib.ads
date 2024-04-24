-- Deklaracja pakietu zawierającego strukturę rozwiązania równania diofantycznego

package Struct_Lib is

   -- Typ rozwiązania równania diofantycznego
   type Solution is record
      X : Integer;
      Y : Integer;
      Err : Boolean := False;
   end record;

end Struct_Lib;
