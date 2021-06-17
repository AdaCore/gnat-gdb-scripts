from support.build import gnatmake
from support.gdb import GDBSession


gnatmake('foo')
gdb = GDBSession('foo')
gdb.run_to(gdb.find_loc('foo.adb', 'BREAK'))

gdb.test('set lang c',
         'Warning: the current language does not match this frame.')
gdb.test('set corrupted_string.P_ARRAY = 0x1', '')
gdb.test('set lang auto', '')

gdb.print_expr('null_string', '(foo.string_access) 0x0 [Invalid]')
gdb.print_expr('empty_string', '(foo.string_access) @ADDRESS ""')
gdb.print_expr('some_string', '(foo.string_access) @ADDRESS "Hello, world!"')
gdb.print_expr('binary_string',
               '(foo.string_access) @ADDRESS "b["00"]""["ff"]"')
gdb.print_expr('wstring',
               '(foo.wstring_access) @ADDRESS "wide string"')
gdb.print_expr('wwstring',
               '(foo.wwstring_access) @ADDRESS "wide wide string"')
gdb.print_expr('corrupted_string', '(foo.string_access) 0x1 [Invalid]')

gdb.test('python'
         ' i = gdb.parse_and_eval("i");'
         ' v = gnatdbg.strings.StringAccess(i)',

         '@...TypeError: Input is not an access to string@...')
