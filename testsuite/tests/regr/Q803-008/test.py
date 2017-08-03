from support.build import gnatmake
from support.gdb import GDBSession


gnatmake('main')
gdb = GDBSession('main')
gdb.run_to(gdb.find_loc('main.adb', 'break here'))
gdb.test('source custom.py', '')
gdb.print_expr(
    'v', '(t => time, vec => main.time_vectors.vector of length 1 = {time})'
)
