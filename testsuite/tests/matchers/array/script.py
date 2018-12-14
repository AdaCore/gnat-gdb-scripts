import re

from gnatdbg.generics import Match


s = gdb.parse_and_eval('s')
char_type = s.type.target()

# Only arrays can match
assert not Match.Array().match(s.type.target())
assert Match.Array().match(s.type)

# The element type must match
assert not Match.Array(element=Match.Array()).match(s.type)
assert Match.Array(element=Match.Char()).match(s.type)

# The first bound must match
assert not Match.Array(first=2).match(s.type)
assert Match.Array(first=1).match(s.type)

# The last bound must match
assert not Match.Array(last=1).match(s.type)
assert Match.Array(last=2).match(s.type)
