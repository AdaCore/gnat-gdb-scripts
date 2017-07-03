with P; use P;

procedure Foo is

   N : Natural := 0;

   Object       : aliased Child_Type := (I => 1, J => 2, K => 3);
   Object_Class : aliased Child_Type'Class := Object;

   type Interface_Access is access all Interface_Type'Class;

   C : access Child_Type'Class := Object'Access;
   R : access Root_Type'Class := Object'Access;
   I : access Interface_Type'Class := Object'Access;

   Objects : array (1 .. 2) of Interface_Access :=
     (Object'Access, Object'Access);

begin
   I.Foo;
   for O of Objects loop
      O.Foo; --  BREAK
   end loop;
   N := 1;
end Foo;
