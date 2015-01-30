import itertools

from gnat_runtime.generics import Match
from gnat_runtime.utils import PrettyPrinter


class DoublyLinkedListPrinter(PrettyPrinter):
    """Pretty-print Ada.Containers.Doubly_Linked_Lists values."""

    name            = 'Doubly_Linked_List'
    generic         = 'ada.containers.doubly_linked_lists'
    type_tag_suffix = 'list'

    node_pattern    = Match.TypeName(
        suffix='__node_type', pattern=Match.Struct(
            Match.Field('element'),
            Match.Field('next', Match.Pointer()),
            Match.Field('prev', Match.Pointer()),
        )
    )

    type_pattern    = Match.TypeName(suffix='__list', pattern=Match.Struct(
        Match.Field('_parent'),
        Match.Field('first',  Match.Pointer(node_pattern)),
        Match.Field('last',   Match.Pointer(node_pattern)),
        Match.Field('length', Match.Integer()),
        Match.Field('busy',   Match.Integer()),
        Match.Field('lock',   Match.Integer()),
    ))

    @property
    def length(self):
        return self.value['length']

    def children(self):
        if self.value['first']:
            node = self.value['first']
            for i in itertools.count(0):
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

    generic         = 'ada.containers.doubly_linked_lists'
    type_tag_suffix = 'cursor'

    type_pattern    = Match.TypeName(suffix='__cursor', pattern=Match.Struct(
        Match.Field('container',
            Match.Pointer(DoublyLinkedListPrinter.type_pattern)),
        Match.Field('node', Match.Pointer()),
    ))

    def to_string(self):
        if self.value['container']:
            assoc = '{}'.format(self.value['node']['element'])
        else:
            assoc = 'No_Element'
        return 'Cursor ({})'.format(assoc)