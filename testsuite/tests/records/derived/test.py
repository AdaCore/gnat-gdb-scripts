from support.build import gnatmake
from support.gdb import GDBSession


gnatmake('foo')
gdb = GDBSession('foo')
gdb.run_to(gdb.find_loc('foo.adb', 'BREAK'))
gdb.test('source helpers.py', '')
gdb.test('pi print_record("R", gdb.parse_and_eval("R"))',
         '== R ==\n'
         '  i: 1\n'
         '  c: 65 \'A\'')
