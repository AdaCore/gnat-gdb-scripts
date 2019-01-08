with Ada.Containers.Doubly_Linked_Lists;

package body Foo.Bar is
   package My_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Integer);

   L : My_Lists.List;

   procedure Dummy is begin
      L.Append (1);
      L.Append (2);
      L.Append (3); --  BREAK
   end;
end Foo.Bar;
