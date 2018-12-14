with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Foo is

   function "+" (S : String) return Unbounded_String
      renames To_Unbounded_String;

   package Str_Lists is
      new Ada.Containers.Doubly_Linked_Lists (Unbounded_String);

   L   : Str_Lists.List;
   Cur : Str_Lists.Cursor := Str_Lists.No_Element;

begin
   L.Append (+"one");
   L.Append (+"two");
   L.Append (+"three");
   Cur := L.First; --  BREAK
end Foo;
