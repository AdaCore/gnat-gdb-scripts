from support.build import gnatmake
from support.gdb import GDBSession


gnatmake('foo')

# Run gnatdbg.setup twice, check that the pretty-printers still work
gdb = GDBSession('foo', log_file='gdb-double-setup.log', load_gnatdbg=True)
gdb.run_to(gdb.find_loc('foo.adb', 'BREAK'))
gdb.test('python gnatdbg.setup()', '')
gdb.print_expr('empty_string', '""')
gdb.stop()

# Run gnatdbg.setup without registering pretty-printers globally
gdb = GDBSession('foo', log_file='gdb-local.log', load_gnatdbg=False)
gdb.run_to(gdb.find_loc('foo.adb', 'BREAK'))
gdb.test('python import gnatdbg; gnatdbg.setup(globally=False)', '')
gdb.print_expr('empty_string', '""')

# Re-load and re-run foo, just to make sure the new_objfile event is run.
gdb.kill()
gdb.run_to(gdb.find_loc('foo.adb', 'BREAK'))
gdb.print_expr('empty_string', '""')
