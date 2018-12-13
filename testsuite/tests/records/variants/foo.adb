procedure Foo is

    type Discr_Record (N : Natural) is record
       Index : Positive;
       case N is
          when 0      => null;
          when 1      => B : Boolean;
          when 2 .. 9 => C : Character;
          when others =>

             case N is
                when 0 .. 9              => null;
                when 10 .. 19 | 90 .. 99 => I : Integer;
                when others              => null;
             end case;
       end case;
    end record;

    procedure Process (R : access constant Discr_Record) is
    begin
       null;
    end Process;

    type Integer_Option (Present : Boolean) is record
       case Present is
          when True  => Value : Integer;
          when False => null;
       end case;
    end record;

    procedure Corrupt (R : access Integer_Option) is
       Discr : Character with Import, Address => R.Present'Address;
    begin
       Discr := 'A';
    end Corrupt;

    R0  : aliased constant Discr_Record := (N => 0, Index => 100);
    R1  : aliased constant Discr_Record := (N => 1, Index => 101, B => True);
    R2  : aliased constant Discr_Record := (N => 2, Index => 102, C => 'A');
    R11 : aliased constant Discr_Record := (N => 11, Index => 111, I => 42);
    R20 : aliased constant Discr_Record := (N => 20, Index => 120);
    RError : aliased Integer_Option := (Present => True, Value => 1);

    R0_Access  : access constant Discr_Record := R0'Access;
    R1_Access  : access constant Discr_Record := R1'Access;
    R2_Access  : access constant Discr_Record := R2'Access;
    R11_Access : access constant Discr_Record := R11'Access;
    R20_Access : access constant Discr_Record := R20'Access;
    RError_Access : access Integer_Option := RError'Access;

begin
   Corrupt (RError_Access);
   Process (R0_Access); --  BREAK
   Process (R1_Access);
   Process (R2_Access);
   Process (R11_Access);
   Process (R20_Access);
end Foo;
