with Ada.Containers.Doubly_Linked_Lists;
with Foo.Bar;

procedure Lister is
   package Main_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Integer);

   ML : Main_Lists.List;
begin
   Foo.Bar.Dummy;
end;
