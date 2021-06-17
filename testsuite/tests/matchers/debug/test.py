from support.build import gnatmake
from support.gdb import GDBSession


gnatmake('foo')
gdb = GDBSession('foo')
gdb.run_to(gdb.find_loc('foo.adb', 'BREAK'))

gdb.test('source script.py', """\
Debug: mismatch:
  Struct(2 fields) <-> TYPE_CODE_STRUCT (name=foo.my_record, 2 fields)
  Field(name=n) <-> n
  TypeName(name=natural) <-> TYPE_CODE_RANGE\
 (name=positive@/(___XDLU_1__.*)?/)""")
