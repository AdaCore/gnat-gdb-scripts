"""
Pretty-printers for hashed and ordered sets in Ada.Containers.
"""

from gnatdbg.generics import Match
from gnatdbg.hash_tables import iterate, get_htable_pattern
from gnatdbg.printers import PrettyPrinter
from gnatdbg.red_black_trees import dfs, get_rbtree_pattern
from gnatdbg.utils import pretty_typename


class BaseSetPrinter(PrettyPrinter):
    """Base class for set pretty-printers."""

    # TODO: GDB has no 'set' display hint yet. Update this once it is the case!

    def children(self):
        for i, node in enumerate(self.get_node_iterator()):
            yield ('[{}]'.format(i), node['element'])

    def to_string(self):
        return '{} of length {}'.format(
            pretty_typename(self.value.type),
            self.length
        )


class OrderedSetPrinter(BaseSetPrinter):
    """Pretty-print Ada.Containers.Ordered_Sets.Set values."""

    name            = 'Ordered_Set'

    type_pattern    = Match.TypeName(suffix='.set', pattern=Match.Struct(
        Match.Field('_parent'),
        Match.Field('tree', get_rbtree_pattern(Match.Struct(
            Match.Field('parent', Match.Typedef(Match.Pointer())),
            Match.Field('left',   Match.Typedef(Match.Pointer())),
            Match.Field('right',  Match.Typedef(Match.Pointer())),
            Match.Field('color',  Match.Enum()),
            Match.Field('element'),
        ))),
    ))

    @property
    def length(self):
        return self.value['tree']['length']

    def get_node_iterator(self):
        return dfs(self.value['tree'])


class OrderedSetCursorPrinter(PrettyPrinter):
    """Pretty-print Ada.Containers.Ordered_Sets.Cursor values."""

    name            = 'Ordered_Set_Cursor'

    type_pattern    = Match.TypeName(suffix='.cursor', pattern=Match.Struct(
        Match.Field('container',
                    Match.Pointer(OrderedSetPrinter.type_pattern)),
        Match.Field('node', Match.Pointer()),
    ))

    def to_string(self):
        if self.value['container']:
            assoc = self.value['node']['element']
        else:
            assoc = 'No_Element'
        return 'Cursor ({})'.format(assoc)


class HashedSetPrinter(BaseSetPrinter):
    """Pretty-print Ada.Containers.Hashed_Sets.Set values."""

    name            = 'Hashed_Set'

    type_pattern    = Match.TypeName(suffix='.set', pattern=Match.Struct(
        Match.Field('_parent'),
        Match.Field('ht', get_htable_pattern(Match.Struct(
            Match.Field('element'),
            Match.Field('next', Match.Typedef(Match.Pointer())),
        ))),
    ))

    @property
    def length(self):
        return self.value['ht']['length']

    def get_node_iterator(self):
        return iterate(self.value['ht'])


class HashedSetCursorPrinter(PrettyPrinter):
    """Pretty-print Ada.Containers.Hashed_Sets.Cursor values."""

    name            = 'Ordered_Set_Cursor'

    type_pattern    = Match.TypeName(suffix='.cursor', pattern=Match.Struct(
        Match.Field('container',
                    Match.Pointer(HashedSetPrinter.type_pattern)),
        Match.Field('node', Match.Pointer()),
        Match.Field('position',
                    Match.TypeName(name='ada.containers.hash_type')),
    ))

    def to_string(self):
        if self.value['container']:
            assoc = self.value['node']['element']
        else:
            assoc = 'No_Element'
        return 'Cursor ({})'.format(assoc)
