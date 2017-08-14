import re

from gnatdbg.generics import Match
from gnatdbg.printers import GDBPrettyPrinters, PrettyPrinter


class MyIntPrinter(PrettyPrinter):
    name = 'My Int'
    type_pattern = Match.TypeName(name=re.compile(r'foo\.my_int_\d+'))

    def to_string(self):
        return 'My_Int ({})'.format(int(self.value))


printers = GDBPrettyPrinters('test')
printers.append(MyIntPrinter)
gdb.selected_frame().function().symtab.objfile.pretty_printers.append(printers)
