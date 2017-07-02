procedure Foo is

   package P is
      type Interface_Type is interface;

      procedure Foo (I : Interface_Type) is abstract;

      type Root_Type is abstract tagged record
         I : Integer;
      end record;

      type Child_Type is new Root_Type and Interface_Type with record
         J, K : Integer;
      end record;

      procedure Foo (I : Child_Type);
   end P;

   package body P is
      procedure Foo (I : Child_Type) is
      begin
         null;
      end Foo;
   end P;

   use P;

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
