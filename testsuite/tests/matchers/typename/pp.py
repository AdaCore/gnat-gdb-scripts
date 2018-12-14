import re

from gnatdbg.generics import Match
from gnatdbg.printers import GDBPrettyPrinters, PrettyPrinter


class MyIntPrinter(PrettyPrinter):
    name = 'My Int'
    type_pattern = Match.TypeName(name=re.compile(r'foo\.my_int_\d+'))

    def to_string(self):
        return 'My_Int ({})'.format(int(self.value))


class MyNatPrinter(PrettyPrinter):
    name = 'My Nat'
    type_pattern = Match.TypeName(name='foo.my_nat')

    def to_string(self):
        return 'My_Nat ({})'.format(int(self.value))


printers = GDBPrettyPrinters('test')
printers.append(MyIntPrinter)
printers.append(MyNatPrinter)
gdb.selected_frame().function().symtab.objfile.pretty_printers.append(printers)
