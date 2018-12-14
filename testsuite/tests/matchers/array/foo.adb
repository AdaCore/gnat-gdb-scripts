procedure Foo is

   procedure Process (S : out String) is
   begin
      S := (S'Range => 'A');
   end Process;

   S : String (1 .. 2) := "AB";

begin
   Process (S); --  BREAK
end Foo;
