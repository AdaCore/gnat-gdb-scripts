import re

from gnatdbg.generics import Match


value = gdb.parse_and_eval('r')
type_matcher = Match.Struct(
    Match.Field('i'),
    Match.Field('n', Match.TypeName(name='natural')),
)
assert not type_matcher.match(value.type, debug=True)
