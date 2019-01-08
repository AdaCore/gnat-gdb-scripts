"""
Pretty-printers for doubly-linked lists in Ada.Containers.Doubly_Linked_Lists.
"""

import itertools

import gdb

from gnatdbg.generics import Match
from gnatdbg.printers import PrettyPrinter
from gnatdbg.utils import pretty_typename


class DoublyLinkedListPrinter(PrettyPrinter):
    """Pretty-print Ada.Containers.Doubly_Linked_Lists values."""

    name            = 'Doubly_Linked_List'

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
            for i in range(self.length):
                yield ('[{}]'.format(i), node['element'])
                node = node['next']
            if node:
                raise gdb.MemoryError('The linked list seems invalid')

    def to_string(self):
        return '{} of length {}'.format(
            pretty_typename(self.value.type),
            self.length,
        )


class DoublyLinkedListCursorPrinter(PrettyPrinter):
    """Pretty-print Ada.Containers.Doubly_Linked_Lists.Cursor values."""

    name            = 'Doubly_Linked_List_Cursor'

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
