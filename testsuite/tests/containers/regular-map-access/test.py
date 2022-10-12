"""
Check that pretty-printers for indefinite maps do not apply to non-indefinite
maps.

A possible way to check for indefinite maps in pretty printers is to test that
the name of key and element types end with "_access". This strategy has an
issue: the key and element type for non-indefinite maps may legitimately have
names that end with "_access": in this case, we do not want the maps
pretty-printers to dereference keys/elements, as this may interfere with their
own pretty-printing. This test checks that this works as expected.
"""

from support.build import gnatmake
from support.gdb import GDBSession


gnatmake("main")
gdb = GDBSession("main")
gdb.test("source pp.py", "")
gdb.run_to(gdb.find_loc("main.adb", "BREAK"))
gdb.print_expr(
    "passwords",
    "main.string_maps.map of length 3 = "
    '{[Name("Alice")] = Name("foo"),'
    ' [Name("Bob")] = Name("baz"),'
    ' [Name("John")] = Name("bar")}'
)
