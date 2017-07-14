with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Foo is
   function "+" (S : String) return Unbounded_String
      renames To_Unbounded_String;
   function Hash (S : Unbounded_String) return Hash_Type is
     (Ada.Strings.Hash (To_String (S)));

   package Str_Vectors is
      new Ada.Containers.Vectors (Positive, Unbounded_String);
   package Str_Lists is
      new Ada.Containers.Doubly_Linked_Lists (Unbounded_String);

   package Int_To_Str is
      new Ada.Containers.Ordered_Maps (Integer, Unbounded_String);
   package Int_Sets is
      new Ada.Containers.Ordered_Sets (Integer);

   package Str_To_Int is
      new Ada.Containers.Hashed_Maps (Unbounded_String, Integer, Hash, "=");
   package Str_Sets is
      new Ada.Containers.Hashed_Sets (Unbounded_String, Hash, "=");

   procedure Break is
   begin
       null; --  BREAK
   end Break;

   US1, US2 : Unbounded_String;

   V1, V2          : Str_Vectors.Vector;
   L1, L2          : Str_Lists.List;
   Cur_V, No_Cur_V : Str_Vectors.Cursor := Str_Vectors.No_Element;
   Cur_L, No_Cur_L : Str_Lists.Cursor := Str_Lists.No_Element;

   OM1, OM2          : Int_To_Str.Map;
   OS1, OS2          : Int_Sets.Set;
   Cur_OM, No_Cur_OM : Int_To_Str.Cursor := Int_To_Str.No_Element;
   Cur_OS, No_Cur_OS : Int_Sets.Cursor := Int_Sets.No_Element;

   HM1, HM2          : Str_To_Int.Map;
   HS1, HS2          : Str_Sets.Set;
   Cur_HM, No_Cur_HM : Str_To_Int.Cursor := Str_To_Int.No_Element;
   Cur_HS, No_Cur_HS : Str_Sets.Cursor := Str_Sets.No_Element;

begin
   US1 := +"Hello world!";
   V1.Append (+"one");
   V1.Append (+"two");
   V1.Append (+"three");

   for Cur in V1.Iterate loop
      declare
         N : constant Positive := Str_Vectors.To_Index (Cur);
         S : constant Unbounded_String := Str_Vectors.Element (Cur);
      begin
         L1.Append (S);
         OM1.Insert (N, S);
         OS1.Insert (N);
         HM1.Insert (S, N);
         HS1.Insert (S);
      end;
   end loop;

   Cur_V := V1.To_Cursor (2);
   Cur_L := L1.First;
   Cur_OM := OM1.Find (2);
   Cur_OS := OS1.Find (2);
   Cur_HM := HM1.Find (+"two");
   Cur_HS := HS1.Find (+"two");

   Break;
end Foo;
