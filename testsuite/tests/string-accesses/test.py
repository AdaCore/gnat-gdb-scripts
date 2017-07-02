from support.build import gnatmake
from support.gdb import GDBSession


gnatmake('foo', ['-O0', '-g'])
gdb = GDBSession('foo')
gdb.run_to(gdb.find_loc('foo.adb', 'BREAK'))

gdb.test('set lang c',
         'Warning: the current language does not match this frame.')
gdb.test('set corrupted_string.P_ARRAY = 0x1', '')
gdb.test('set lang auto', '')

gdb.print_expr('null_string', '(foo__string_access) 0x0 [Invalid]')
gdb.print_expr('empty_string', '(foo__string_access) @ADDRESS ""')
gdb.print_expr('some_string', '(foo__string_access) @ADDRESS "Hello, world!"')
gdb.print_expr('binary_string', '(foo__string_access) @ADDRESS "b["00"]""["ff"]"')
gdb.print_expr('corrupted_string', '(foo__string_access) 0x1 [Invalid]')

gdb.test('python'
         ' i = gdb.parse_and_eval("i");'
         ' v = gnatdbg.strings.StringAccess(i)',

         '@...TypeError: Input is not an access to string@...')
