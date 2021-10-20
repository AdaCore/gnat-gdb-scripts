from support.build import gnatmake
from support.gdb import GDBSession


gnatmake('foo')
gdb = GDBSession('foo')
gdb.run_to(gdb.find_loc('foo.adb', 'BREAK'))

gdb.execute('set corrupted.value.c := 0x1')

gdb.print_expr('uninit', 'Big_Integer ([Uninitialized])')
gdb.print_expr('zero', 'Big_Integer (0)')
gdb.print_expr('neg_one', 'Big_Integer (-1)')
gdb.print_expr('neg_small', 'Big_Integer (-1000)')
gdb.print_expr(
    'neg_big',
    'Big_Integer (-1234567890098765432112345678900987654321)'
)
gdb.print_expr('pos_one', 'Big_Integer (1)')
gdb.print_expr('pos_small', 'Big_Integer (1000)')
gdb.print_expr(
    'pos_big',
    'Big_Integer (1234567890098765432112345678900987654321)'
)
gdb.print_expr('corrupted', 'Big_Integer ([Invalid])')
