from support.build import gnatmake
from support.gdb import GDBSession


gnatmake('foo')
gdb = GDBSession('foo')
gdb.run_to(gdb.find_loc('foo.adb', 'BREAK'))
gdb.test('source script.py', """\
Trying to access element 4 of "V"...
   gdb.MemoryError: Out of bound vector access (4 not in 1 ..  3)
Trying to print vector at address 0...
   gdb.MemoryError: Cannot access memory at address 0x0
Trying to print corrupted cursor...
   Cursor ([Invalid])""")
