from support.build import gnatmake
from support.gdb import GDBSession


gnatmake('foo', ['-O0', '-g'])
gdb = GDBSession('foo')
gdb.run_to(gdb.find_loc('foo.adb', 'BREAK'))

gdb.execute('python import gnatdbg.debug')
gdb.execute('python gnatdbg.debug.PrintGDBTypeTreeCommand()')

gdb.test('dbgtype i', '%0 (integer) = TYPE_CODE_INT (4 bytes)')

gdb.test('dbgtype l', '%0 (foo__linked_list) = TYPE_CODE_TYPEDEF:'
                      '  %1 (None) = TYPE_CODE_PTR:'
                      '    %2 (foo__linked_list_record) = TYPE_CODE_STRUCT:'
                      '      value: %3 (integer) = TYPE_CODE_INT (4 bytes)'
                      '      next: %0 (foo__linked_list)')

gdb.test('dbgtype s', '%0 (foo__TsS) = TYPE_CODE_ARRAY[1 .. 13]:'
                      '  %1 (character) = TYPE_CODE_CHAR')
