import itertools

import gdb

from gnatdbg.generics import Match
from gnatdbg.printers import PrettyPrinter


class DoublyLinkedListPrinter(PrettyPrinter):
    """Pretty-print Ada.Containers.Doubly_Linked_Lists values."""

    name            = 'Doubly_Linked_List'
    type_tag_suffix = 'list'

    node_pattern    = Match.TypeName(
        suffix='.node_type', pattern=Match.Struct(
            Match.Field('element'),
            Match.Field('next', Match.Pointer()),
            Match.Field('prev', Match.Pointer()),
        )
    )

    type_pattern    = Match.TypeName(suffix='.list', pattern=Match.Struct(
        Match.Field('_parent'),
        Match.Field('first',  Match.Pointer(node_pattern)),
        Match.Field('last',   Match.Pointer(node_pattern)),
        Match.Field('length', Match.Integer()),
        Match.Field('tc'),
    ))

    def display_hint(self):
        return 'array'

    @property
    def length(self):
        return self.value['length']

    def children(self):
        if self.value['first']:
            node = self.value['first']
            for i in itertools.count(0):
                if i >= self.length:
                    raise gdb.MemoryError('The linked list seems invalid')
                yield ('[{}]'.format(i), node['element'])
                if node == self.value['last']:
                    break
                node = node['next']

    def to_string(self):
        return '{} of length {}'.format(
            str(self.value.type),
            self.length,
        )


class DoublyLinkedListCursorPrinter(PrettyPrinter):
    """Pretty-print Ada.Containers.Doubly_Linked_Lists.Cursor values."""

    name            = 'Doubly_Linked_List_Cursor'

    type_tag_suffix = 'cursor'

    type_pattern    = Match.TypeName(suffix='.cursor', pattern=Match.Struct(
        Match.Field('container',
                    Match.Pointer(DoublyLinkedListPrinter.type_pattern)),
        Match.Field('node', Match.Pointer()),
    ))

    def display_hint(self):
        return 'array'

    def to_string(self):
        if self.value['container']:
            try:
                assoc = '{}'.format(self.value['node']['element'])
            except gdb.MemoryError:
                return 'Cursor ([Invalid])'
        else:
            assoc = 'No_Element'
        return 'Cursor ({})'.format(assoc)
