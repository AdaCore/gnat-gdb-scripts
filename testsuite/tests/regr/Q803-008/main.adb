with Ada.Containers.Vectors;
with Universal_Calendar;     use Universal_Calendar;

procedure Main is

   package Time_Vectors is new Ada.Containers.Vectors (Positive, Time);

   type R is record
      T   : Time;
      Vec : Time_Vectors.Vector;
   end record;

   V : R;
   Dummy : Integer;
begin
   V.T := 1;
   V.Vec.Append (1);

   Dummy := 1;   --  break here
end Main;
