from support.build import gnatmake
from support.gdb import GDBSession


gnatmake('foo')
gdb = GDBSession('foo')
gdb.run_to(gdb.find_loc('foo.adb', 'BREAK'))

# Corrupt the linked list and the cursor
gdb.test('set variable l.length := 2', '')
gdb.test('set variable cur.container := 1', '')
gdb.test('set variable cur.node := 2', '')

gdb.print_expr(
    'l',
    'foo.str_lists.list of length 2 ='
    ' {Unbounded_String ("one"),'
    ' Unbounded_String ("two")'
    '<error reading variable: The linked list seems invalid>...}'
)
gdb.print_expr('cur', 'Cursor ([Invalid])')
