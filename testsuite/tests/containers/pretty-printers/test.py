from support.build import gnatmake
from support.gdb import GDBSession


gnatmake('foo')
gdb = GDBSession('foo')
gdb.run_to(gdb.find_loc('foo.adb', 'BREAK'))

gdb.print_expr('us1', 'Unbounded_String ("Hello world!")')
gdb.print_expr('us2', 'Unbounded_String ("")')

gdb.print_expr('v1', 'foo.str_vectors.vector of length 3 ='
                     ' {Unbounded_String ("one"),'
                     '  Unbounded_String ("two"),'
                     '  Unbounded_String ("three")}')
gdb.print_expr('v2', 'foo.str_vectors.vector of length 0')
gdb.print_expr('cur_v', 'Cursor (2 => Unbounded_String ("two"))')
gdb.print_expr('no_cur_v', 'Cursor (No_Element)')

gdb.print_expr('l1', 'foo.str_lists.list of length 3 ='
                     ' {Unbounded_String ("one"),'
                     '  Unbounded_String ("two"),'
                     '  Unbounded_String ("three")}')
gdb.print_expr('l2', 'foo.str_lists.list of length 0')
gdb.print_expr('cur_l', 'Cursor (Unbounded_String ("one"))')
gdb.print_expr('no_cur_l', 'Cursor (No_Element)')

gdb.print_expr('om1', 'foo.int_to_str.map of length 3 ='
                      ' {[1] = Unbounded_String ("one"),'
                      '  [2] = Unbounded_String ("two"),'
                      '  [3] = Unbounded_String ("three")}')
gdb.print_expr('om2', 'foo.int_to_str.map of length 0')
gdb.print_expr('cur_om', 'Cursor (2 => Unbounded_String ("two"))')
gdb.print_expr('no_cur_om', 'Cursor (No_Element)')

gdb.print_expr('hm1', 'foo.str_to_int.map of length 3 ='
                      ' {[Unbounded_String ("one")] = 1,'
                      '  [Unbounded_String ("three")] = 3,'
                      '  [Unbounded_String ("two")] = 2}')
gdb.print_expr('hm2', 'foo.str_to_int.map of length 0')
gdb.print_expr('cur_hm', 'Cursor (Unbounded_String ("two") => 2)')
gdb.print_expr('no_cur_hm', 'Cursor (No_Element)')

gdb.print_expr('os1', 'foo.int_sets.set of length 3 ='
                      ' {[0] = 1, [1] = 2, [2] = 3}')
gdb.print_expr('os2', 'foo.int_sets.set of length 0')
gdb.print_expr('cur_os', 'Cursor (2)')
gdb.print_expr('no_cur_os', 'Cursor (No_Element)')

gdb.print_expr('hs1', 'foo.str_sets.set of length 3 ='
                      ' {[0] = Unbounded_String ("one"),'
                      '  [1] = Unbounded_String ("three"),'
                      '  [2] = Unbounded_String ("two")}')
gdb.print_expr('hs2', 'foo.str_sets.set of length 0')
gdb.print_expr('cur_hs', 'Cursor (Unbounded_String ("two"))')
gdb.print_expr('no_cur_hs', 'Cursor (No_Element)')
