from support.build import gnatmake
from support.gdb import GDBSession


gnatmake('foo')
gdb = GDBSession('foo')
gdb.run_to(gdb.find_loc('foo.adb', 'BREAK'))

gdb.test('set lang c',
         'Warning: the current language does not match this frame.')
gdb.test('set corrupted_string.P_ARRAY = 0x1', '')
gdb.test('set lang auto', '')

gdb.print_expr('null_string', '0x0 <null string access>')
gdb.print_expr('empty_string', '""')
gdb.print_expr('some_string', '"Hello, world!"')
gdb.print_expr('binary_string', '"b["00"]""["ff"]"')
gdb.print_expr('wstring', '"wide string"')
gdb.print_expr('wwstring', '"wide wide string"')
gdb.print_expr('corrupted_string', '<error: Cannot access memory at address 0x1>')

gdb.test('python'
         ' i = gdb.parse_and_eval("i");'
         ' v = gnatdbg.strings.StringAccess(i)',

         '@...TypeError: Input is not an access to string@...')
