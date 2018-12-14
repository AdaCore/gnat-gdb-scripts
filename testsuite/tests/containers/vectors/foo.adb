with Ada.Containers.Vectors;

procedure Foo is

   package Int_Vectors is new Ada.Containers.Vectors (Positive, Integer);

   V   : Int_Vectors.Vector;
   Cur : Int_Vectors.Cursor := Int_Vectors.No_Element;

begin
   for I in 1 .. 3 loop
      V.Append (I);
   end loop;
   Cur := V.First; --  BREAK
end Foo;
