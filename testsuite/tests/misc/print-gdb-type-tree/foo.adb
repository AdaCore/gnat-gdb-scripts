procedure Foo is
   I : Integer := 0;

   type Linked_List_Record;
   type Linked_List is access Linked_List_Record;
   type Linked_List_Record is record
      Value : Integer;
      Next  : Linked_List;
   end record;

   L : Linked_List := null;

   S : String := "Hello, world!";

begin
   I := 1; --  BREAK
   L := new Linked_List_Record'(Value => 1, Next => null);
end Foo;
