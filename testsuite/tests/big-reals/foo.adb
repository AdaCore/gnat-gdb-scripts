pragma Ada_2022;

with Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Numerics.Big_Numbers.Big_Reals;
use Ada.Numerics.Big_Numbers.Big_Reals;

procedure Foo is
   Uninit, Zero       : Big_Real;
   Neg_One, Neg_Small : Big_Real;
   Pos_One, Pos_Small : Big_Real;
   Corrupted          : Big_Real;
begin
   Zero := 0 / 5;
   Neg_One := -1 / 1;
   Neg_Small := 10 / (-9);
   Pos_One := 6 / 6;
   Pos_Small := 1000 / 3;
   Corrupted := Pos_One;
   Corrupted := Zero;  --  BREAK
end Foo;
