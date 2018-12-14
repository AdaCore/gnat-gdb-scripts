with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
procedure Foo is
   Empty_String     : Unbounded_String;
begin
   Empty_String := To_Unbounded_String (""); --  BREAK
end Foo;
