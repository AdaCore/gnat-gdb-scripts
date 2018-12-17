procedure Foo is

   type Base is tagged record
      I : Integer;
   end record;

   type Derived is new Base with record
      C : Character;
   end record;

   procedure Process (R : Base'Class) is
   begin
      null;
   end Process;

   R : Derived := (I => 1, C => 'A');

begin
   Process (R); --  BREAK
end Foo;
