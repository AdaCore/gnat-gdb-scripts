pragma Ada_2022;

with Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Numerics.Big_Numbers.Big_Integers;

procedure Foo is
   Uninit, Zero                : Big_Integer;
   Neg_One, Neg_Small, Neg_Big : Big_Integer;
   Pos_One, Pos_Small, Pos_Big : Big_Integer;
   Corrupted                   : Big_Integer;
begin
   Zero := 0;
   Neg_One := -1;
   Neg_Small := -1000;
   Neg_Big := -1234567890_0987654321_1234567890_0987654321;
   Pos_One := 1;
   Pos_Small := 1000;
   Pos_Big := 1234567890_0987654321_1234567890_0987654321;
   Corrupted := Zero;  --  BREAK
end Foo;
