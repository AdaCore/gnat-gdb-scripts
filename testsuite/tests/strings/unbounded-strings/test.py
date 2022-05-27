from support.build import gnatmake
from support.gdb import GDBSession


gnatmake('foo')
gdb = GDBSession('foo')
gdb.run_to(gdb.find_loc('foo.adb', 'BREAK'))

gdb.execute('set corrupted_string.reference := 0x1')

gdb.print_expr('empty_string', '""')
gdb.print_expr('some_string', '"Hello, world!"')
gdb.print_expr('binary_string', '"b["00"]""["ff"]"')
gdb.print_expr('corrupted_string', '<error reading variable: Cannot access memory at address 0x1>')
