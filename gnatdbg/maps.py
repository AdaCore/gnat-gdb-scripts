"""
Pretty-printers for hashed and ordered maps in Ada.Containers.
"""

from itertools import count

from gnatdbg.generics import Match
from gnatdbg.hash_tables import iterate, get_htable_pattern
from gnatdbg.printers import PrettyPrinter
from gnatdbg.red_black_trees import dfs, get_rbtree_pattern
from gnatdbg.utils import pretty_typename


class BaseMapPrinter(PrettyPrinter):
    """Base class for map pretty-printers."""

    def display_hint(self):
        return 'map'

    def get_node_iterator(self):
        raise NotImplementedError()

    def children(self):
        # This is an infinite iterator by design: the exit path is not
        # coverable.
        names = iter('[{}]'.format(i) for i in count(0)) # no-code-coverage

        for node in self.get_node_iterator():
            yield (next(names), node['key'])
            yield (next(names), node['element'])

    def to_string(self):
        return '{} of length {}'.format(
            pretty_typename(self.value.type),
            self.length
        )


class OrderedMapPrinter(BaseMapPrinter):
    """Pretty-print Ada.Containers.Ordered_Maps.Map values."""

    name            = 'Ordered_Map'

    type_pattern    = Match.TypeName(suffix='.map', pattern=Match.Struct(
        Match.Field('_parent'),
        Match.Field('tree', get_rbtree_pattern(Match.Struct(
            Match.Field('parent', Match.Pointer()),
            Match.Field('left',   Match.Pointer()),
            Match.Field('right',  Match.Pointer()),
            Match.Field('color',  Match.Enum()),
            Match.Field('key'),
            Match.Field('element'),
        ))),
    ))

    @property
    def length(self):
        return self.value['tree']['length']

    def get_node_iterator(self):
        return dfs(self.value['tree'])


class OrderedMapCursorPrinter(PrettyPrinter):
    """Pretty-print Ada.Containers.Ordered_Maps.Cursor values."""

    name            = 'Ordered_Map_Cursor'

    type_pattern    = Match.TypeName(suffix='.cursor', pattern=Match.Struct(
        Match.Field('container',
                    Match.Pointer(OrderedMapPrinter.type_pattern)),
        Match.Field('node', Match.Pointer()),
    ))

    def to_string(self):
        if self.value['container']:
            assoc = '{} => {}'.format(
                self.value['node']['key'],
                self.value['node']['element'],
            )
        else:
            assoc = 'No_Element'
        return 'Cursor ({})'.format(assoc)


class HashedMapPrinter(BaseMapPrinter):
    """Pretty-print Ada.Containers.Hashed_Maps.Map values."""

    name            = 'Hashed_Map'

    type_pattern    = Match.TypeName(suffix='.map', pattern=Match.Struct(
        Match.Field('_parent'),
        Match.Field('ht', get_htable_pattern(Match.Struct(
            Match.Field('key'),
            Match.Field('element'),
            Match.Field('next', Match.Pointer()),
        ))),
    ))

    @property
    def length(self):
        return self.value['ht']['length']

    def get_node_iterator(self):
        return iterate(self.value['ht'])


class HashedMapCursorPrinter(PrettyPrinter):
    """Pretty-print Ada.Containers.Hashed_Maps.Cursor values."""

    name            = 'Ordered_Map_Cursor'

    type_pattern    = Match.TypeName(suffix='.cursor', pattern=Match.Struct(
        Match.Field('container',
                    Match.Pointer(HashedMapPrinter.type_pattern)),
        Match.Field('node', Match.Pointer()),
        Match.Field('position',
                    Match.TypeName(name='ada.containers.hash_type')),
    ))

    def to_string(self):
        if self.value['container']:
            assoc = '{} => {}'.format(
                self.value['node']['key'],
                self.value['node']['element'],
            )
        else:
            assoc = 'No_Element'
        return 'Cursor ({})'.format(assoc)
