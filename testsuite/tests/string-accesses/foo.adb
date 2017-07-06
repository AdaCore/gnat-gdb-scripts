procedure Foo is
   type String_Access is access String;
   type WString_Access is access Wide_String;
   type WWString_Access is access Wide_Wide_String;

   I : Integer := 0;

   Null_String      : String_Access := null;
   Empty_String     : String_Access := new String'("");
   Some_String      : String_Access := new String'("Hello, world!");
   WString          : WString_Access := new Wide_String'("wide string");
   WWString         : WWString_Access :=
      new Wide_Wide_String'("wide wide string");
   Binary_String    : String_Access :=
      new String'("b" & ASCII.NUL & '"' & Character'Val (255));
   Corrupted_String : String_Access;

   procedure Process (S : String_Access) is
   begin
      if S /= null then
         I := I + S.all'Length;
      end if;
   end Process;

   procedure Process (S : WString_Access) is
   begin
      null;
   end Process;

   procedure Process (S : WWString_Access) is
   begin
      null;
   end Process;

begin
   Process (Null_String); --  BREAK
   Process (Empty_String);
   Process (Some_String);
   Process (Binary_String);
   Process (WString);
   Process (WWString);
   Process (Corrupted_String);
end Foo;
