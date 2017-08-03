procedure Foo is
   type My_Int_1 is new Integer;
   type My_Int_2 is new Integer;

   I1 : My_Int_1 := 10;
   I2 : My_Int_2 := 20;
begin
   I1 := I1 + My_Int_1 (I2); --  BREAK
   I2 := I2 + My_Int_2 (I1);
end Foo;
