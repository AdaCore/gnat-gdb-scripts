from support.build import gnatmake
from support.gdb import GDBSession


gnatmake('foo')
gdb = GDBSession('foo')
gdb.run_to(gdb.find_loc('foo.adb', 'BREAK'))

gdb.test('source pp.py', '')
gdb.print_expr('i1', 'My_Int (10)')
gdb.print_expr('i2', 'My_Int (20)')
