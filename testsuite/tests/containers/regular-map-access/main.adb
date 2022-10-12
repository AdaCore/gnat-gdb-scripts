with Ada.Containers.Ordered_Maps;
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   type Name_Record (Size : Natural) is record
      Value : String (1 .. Size);
   end record;
   type Name_Access is access Name_Record;

   function "<" (Left, Right : Name_Access) return Boolean
   is (Left.Value < Right.Value);

   function "+" (Value : String) return Name_Access
   is (new Name_Record'(Size => Value'Length, Value => Value));

   package String_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Name_Access,
      Element_Type => Name_Access);
   Passwords : String_Maps.Map;
begin
   Passwords.Include (+"Alice", +"foo");
   Passwords.Include (+"John",  +"bar");
   Passwords.Include (+"Bob",   +"baz");

   for C in Passwords.Iterate loop  --  BREAK
      Put_Line
        (String_Maps.Key (C).Value & ": " & String_Maps.Element (C).Value);
   end loop;
end Main;
