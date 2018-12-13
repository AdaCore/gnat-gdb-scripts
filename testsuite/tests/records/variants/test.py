from support.build import gnatmake
from support.gdb import GDBSession


gnatmake('foo')
gdb = GDBSession('foo')
gdb.run_to(gdb.find_loc('foo.adb', 'BREAK'))
gdb.test('source helpers.py', '')


tests = [
    (0, '  n: 0\n'
        '  index: 100'),
    (1, '  n: 1\n'
        '  index: 101\n'
        '  b: true'),
    (2, '  n: 2\n'
        '  index: 102\n'
        '  c: 65 \'A\''),
    (11, '  n: 11\n'
         '  index: 111\n'
         '  i: 42'),
    (20, '  n: 20\n'
         '  index: 120'),

    ('Error', '  present: 65'),
]

for num, expected_content in tests:
    simple_var = 'R{}'.format(num)
    gdb.test('pi print_record({0}, gdb.parse_and_eval({0}))'
             .format(repr(simple_var)),
             '== {} ==\n'
             '{}'.format(simple_var, expected_content))

    access_var = 'R{}_Access'.format(num)
    gdb.test('pi print_record({0}, gdb.parse_and_eval({0}).dereference())'
             .format(repr(access_var)),
             '== {} ==\n'
             '{}'.format(access_var, expected_content))
