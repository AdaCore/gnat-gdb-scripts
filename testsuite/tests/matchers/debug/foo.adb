procedure Foo is
   type My_Record is record
      I : Integer;
      N : Positive;
   end record;

   R : My_Record := (1, 2);
begin
   R.N := R.N + R.I; -- BREAK
end Foo;
