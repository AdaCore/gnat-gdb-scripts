from support.build import gnatmake
from support.gdb import GDBSession


gnatmake('foo')
gdb = GDBSession('foo')
gdb.run_to(gdb.find_loc('foo.adb', 'BREAK'))
gdb.test('source script.py', '')

tests = [
    ('unc_param',
     '== unc_param ==\n'
     '%0 (None : TYPE_CODE_ARRAY)[1 .. 5]:\n'
     '  %1 (character : TYPE_CODE_CHAR)\n'
     '"Hello"'),
    ('unc',
     '== unc ==\n'
     '%0 (foo__run__T@/\\d+/b : TYPE_CODE_ARRAY)[1 .. 10]:\n'
     '  %1 (character : TYPE_CODE_CHAR)\n'
     '"HelloHello"'),
    ('acc',
     '== acc ==\n'
     '%0 (None : TYPE_CODE_ARRAY)[1 .. 5]:\n'
     '  %1 (character : TYPE_CODE_CHAR)\n'
     '"Hello"'),
    ('empty',
     '== empty ==\n'
     '%0 (None : TYPE_CODE_ARRAY)[1 .. 0]:\n'
     '  %1 (character : TYPE_CODE_CHAR)\n'
     '""'),
    ('null_str',
     '== null_str ==\n'
     'None'),
    ('i',
     '== i ==\n'
     '<ValueError: Invalid array value: 0 (TYPE_CODE_INT)>'),
]

for var, expected_output in tests:
    gdb.test('pi run({})'.format(repr(var)),
             expected_output)
