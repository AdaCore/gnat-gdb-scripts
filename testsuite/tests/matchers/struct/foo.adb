procedure Foo is

   type Null_Rec is null record;
   type Rec is record
      I : Integer;
   end record;

   procedure Process (R : out Null_Rec) is
   begin
      R := (null record);
   end Process;

   procedure Process (R : out Rec) is
   begin
      R := (I => 1);
   end Process;

   NR : Null_Rec;
   R  : Rec;
   I : Integer;

begin
   Process (NR); --  BREAK
   Process (R); --  BREAK
   I := I + 1;
end Foo;
