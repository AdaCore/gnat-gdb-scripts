with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
procedure Foo is
   Empty_String     : Unbounded_String;
   Some_String      : Unbounded_String :=
      To_Unbounded_String ("Hello, world!");
   Binary_String    : Unbounded_String :=
      To_Unbounded_String ("b" & ASCII.NUL & '"' & Character'Val (255));
   Corrupted_String : Unbounded_String := To_Unbounded_String ("blah");
begin
   Empty_String := To_Unbounded_String (""); --  BREAK
   Corrupted_String := Some_String;
end Foo;
