with Ada.Tags;                use Ada.Tags;
with Ada.Unchecked_Conversion;
with System;
with System.Storage_Elements; use System.Storage_Elements;

with P; use P;

procedure Foo is

   function Convert is new Ada.Unchecked_Conversion
     (Ada.Tags.Tag, System.Address);

   N : Natural := 0;

   Object       : aliased Child_Type := (I => 1, J => 2, K => 3);
   Object_Class : aliased Child_Type'Class := Object;

   type Interface_Access is access all Interface_Type'Class;

   C : access Child_Type'Class := Object'Access;
   R : access Root_Type'Class := Object'Access;
   I : access Interface_Type'Class := Object'Access;

   Objects : array (1 .. 2) of Interface_Access :=
     (Object'Access, Object'Access);

   type Tag_Header is array (1 .. 4) of System.Address;
   --  Room for tag fields: from "Signature" to "TSD_Ptr", plus one field that
   --  corresponds to the first primitive op pointer (i.e. the actual tag will
   --  point to it).

   Tag_Header_Byte_Size : constant Storage_Offset :=
      Tag_Header'Size / System.Storage_Unit;

   --  Create a copy of the tag header for Child_Type so we can corrupt it

   Tag_Hdr_Addr       : constant System.Address :=
      Convert (C'Tag) - Tag_Header_Byte_Size;
   Tag_Hdr            : constant Tag_Header
      with Import, Address => Tag_Hdr_Addr;
   Corrupted_Tag      : Tag_Header := Tag_Hdr;
   Corrupted_Tag_Addr : System.Address :=
      Corrupted_Tag'Address + Tag_Header_Byte_Size;

begin
   I.Foo;
   for O of Objects loop
      O.Foo; --  BREAK
   end loop;
   N := 1;
end Foo;
