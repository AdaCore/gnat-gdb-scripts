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
