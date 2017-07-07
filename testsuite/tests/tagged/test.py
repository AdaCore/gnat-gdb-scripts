from support.build import gnatmake
from support.gdb import GDBSession


gnatmake('foo')
gdb = GDBSession('foo')
gdb.run_to(gdb.find_loc('foo.adb', 'BREAK'))

gdb.test('python from gnatdbg.tagged import *', '')

gdb.test('python'
         ' c = gdb.parse_and_eval("c");'
         ' r = gdb.parse_and_eval("r");'
         ' i = gdb.parse_and_eval("i");', '')
gdb.test('python'
         ' rc = reinterpret_tagged(c);'
         ' rr = reinterpret_tagged(r);'
         ' ri = reinterpret_tagged(i);', '')

gdb.test('python print("c: {} -> {} {}".format(c.type, rc.type, rc))',
         'c: access p.child_type'
         ' -> p.child_type (i => 1, j => 2, k => 3)')

gdb.test('python print("r: {} -> {} {}".format(r.type, rr.type, rr))',
         'r: access p.root_type'
         ' -> p.child_type (i => 1, j => 2, k => 3)')

gdb.test('python print("i: {} -> {} {}".format(i.type, ri.type, ri))',
         'i: access p.interface_type'
         ' -> p.child_type (i => 1, j => 2, k => 3)')

gdb.test('python tagged_field(gdb.parse_and_eval("n"), "foo")',
         '@...TypeError: Input type is not tagged@...')

gdb.test('python tagged_field(c, "foo")',
         '@...gdb.error: There is no member foo@...')

# Test that FOR loops iteration variables, which are implemented as references,
# are automatically dereferenced.
gdb.test('python print(str(reinterpret_tagged(gdb.parse_and_eval("o"))))',
         '(i => 1, j => 2, k => 3)')

# Corrupt Child_Type's dispatch table, in particular the signature field and
# try to reinterpret "c" once more.
gdb.test('python'
         ' tag_addr, _ = get_dyntype_info(c);'
         ' sign_addr = int(tag_addr - 3 * get_system_address().sizeof - 4);'
         ' gdb.selected_inferior().write_memory(sign_addr, "aaaa")', '')
gdb.test('python print(str(reinterpret_tagged(c)))',
         '@...gdb.MemoryError: Corrupted tag@...')
