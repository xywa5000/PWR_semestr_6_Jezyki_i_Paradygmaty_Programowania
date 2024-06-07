with Ada.Numerics.Float_Random;  use Ada.Numerics.Float_Random;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Containers.Vectors;     use Ada.Containers;

procedure Test_Dining_Philosophers is

   Num_Philosophers : constant := 5;
   Life_Span : constant := 3;

   protected type Fork is
      entry Grab;
      procedure Put_Down;
   private
      Seized : Boolean := False;
   end Fork;
   
   protected body Fork is
      entry Grab when not Seized is
      begin
         Seized := True;
      end Grab;
      procedure Put_Down is
      begin
         Seized := False;
      end Put_Down;
   end Fork;
   
   task type Person (ID : Integer; First, Second : not null access Fork);
   task body Person is
      Dice : Generator;
   begin
      Reset (Dice);
      for Life_Cycle in 1..Life_Span loop
         Put_Line (Integer'Image (ID) & " is thinking");
         delay Duration (Random (Dice) * 0.100);
         Put_Line ("               " & Integer'Image (ID) & " is hungry");
         First.Grab;
         Second.Grab;
         Put_Line ("                              " & Integer'Image (ID) & " is eating his" & Integer'Image (Life_Cycle) & ". meal");
         delay Duration (Random (Dice) * 0.100);
         Second.Put_Down;
         First.Put_Down;
      end loop;
      Put_Line ("                                                            Philosopher" & Integer'Image (ID) & " has finished eating" & Integer'Image (Life_Span) & " meals.");
   end Person;

   Forks : array (1..Num_Philosophers) of aliased Fork;
   Philosophers : array (1..Num_Philosophers) of access Person;

begin

   for I in 1..Num_Philosophers loop
      Philosophers (I) := new Person (I, Forks (I)'Access, Forks ((I mod Num_Philosophers) + 1)'Access);
   end loop;
   null;

end Test_Dining_Philosophers;