import re

from gnatdbg.generics import Match


nr = gdb.parse_and_eval('nr')
r = gdb.parse_and_eval('r')
i = gdb.parse_and_eval('i')

# Only structs can match
assert not Match.Struct().match(i.type)
assert Match.Struct().match(nr.type)

# The number of fields must match
assert not Match.Struct(Match.Field('i')).match(nr.type)
assert Match.Struct(Match.Field('i')).match(r.type)

# The field names must match
assert not Match.Struct(Match.Field('o')).match(r.type)
assert Match.Struct(Match.Field('i')).match(r.type)

# The field types must match
assert not Match.Struct(Match.Field('i', Match.Char())).match(r.type)
assert Match.Struct(Match.Field('i', Match.Integer())).match(r.type)
