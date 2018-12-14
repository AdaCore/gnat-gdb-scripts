procedure Foo is
   type My_Int_1 is new Integer;
   type My_Int_2 is new Integer;

   type My_Nat is new Natural;

   I1 : My_Int_1 := 10;
   I2 : My_Int_2 := 20;
   N  : My_Nat := 30;

   I : Integer := 0;
begin
   I1 := I1 + My_Int_1 (I2); --  BREAK
   I2 := I2 + My_Int_2 (I1);
   N := N + 1;
   I := I + 1;
end Foo;
