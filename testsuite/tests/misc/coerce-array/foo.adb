procedure Foo is

   type String_Access is access all String;

   I : Integer := 0;

   procedure Process (S : String) is
   begin
      null;
   end Process;

   function Null_String return String_Access is
   begin
      return null;
   end Null_String;

   function Alloc (S : String) return String_Access is
   begin
      return new String'(S);
   end Alloc;

   procedure Run (Unc_Param : aliased String) is
      Unc     : constant String := Unc_Param & Unc_Param;
      Con     : constant String (1 .. 10) := (1 .. 10 => 'A');
      Acc     : access constant String := Unc_Param'Access;
      Empty   : constant String_Access := Alloc ("");
      Null_Str : constant String_Access := Null_String;
   begin
      Process (Unc_Param); --  BREAK
      Process (Unc);
      Process (Con);
      Process (Acc.all);
      Process (Empty.all);
      Process (Null_Str.all);
      I := I + 1;
   end Run;

   S : aliased String := "Hello";
begin
   Run (S);
end Foo;
