from support.build import gnatmake
from support.gdb import GDBSession


gnatmake('foo')
gdb = GDBSession('foo')
gdb.run_to(gdb.find_loc('foo.adb', 'BREAK'))

gdb.execute('set corrupted.num.value.c := 0x1')

gdb.print_expr('uninit', 'Big_Real ([Uninitialized])')
gdb.print_expr('zero', 'Big_Real (0 / 1)')
gdb.print_expr('neg_one', 'Big_Real (-1 / 1)')
gdb.print_expr('neg_small', 'Big_Real (-10 / 9)')
gdb.print_expr('pos_one', 'Big_Real (1 / 1)')
gdb.print_expr('pos_small', 'Big_Real (1000 / 3)')
gdb.print_expr('corrupted', 'Big_Real ([Invalid])')
