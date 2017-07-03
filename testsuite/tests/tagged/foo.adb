with P; use P;

procedure Foo is

   N : Natural := 0;

   Object       : aliased Child_Type := (I => 1, J => 2, K => 3);
   Object_Class : aliased Child_Type'Class := Object;

   C : access Child_Type'Class := Object'Access;
   R : access Root_Type'Class := Object'Access;
   I : access Interface_Type'Class := Object'Access;

begin
   I.Foo; --  BREAK
   N := 1;
end Foo;
