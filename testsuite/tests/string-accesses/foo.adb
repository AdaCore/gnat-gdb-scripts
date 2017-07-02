procedure Foo is
   type String_Access is access String;

   I : Integer := 0;

   Null_String      : String_Access := null;
   Empty_String     : String_Access := new String'("");
   Some_String      : String_Access := new String'("Hello, world!");
   Binary_String    : String_Access :=
      new String'("b" & ASCII.NUL & '"' & Character'Val (255));
   Corrupted_String : String_Access;

   procedure Process (S : String_Access) is
   begin
      if S /= null then
         I := I + S.all'Length;
      end if;
   end Process;

begin
   Process (Null_String); --  BREAK
   Process (Empty_String);
   Process (Some_String);
   Process (Binary_String);
   Process (Corrupted_String);
end Foo;
