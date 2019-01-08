from support.build import gnatmake
from support.gdb import GDBSession


gnatmake('lister')
gdb = GDBSession('lister')
gdb.run_to(gdb.find_loc('foo-bar.adb', 'BREAK'))
gdb.print_expr('l', 'foo.bar.my_lists.list of length 2 = {1, 2}')
