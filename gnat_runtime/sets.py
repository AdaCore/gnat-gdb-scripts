from gnat_runtime.generics import Match
from gnat_runtime.hash_tables import iterate, get_htable_pattern
from gnat_runtime.red_black_trees import dfs, get_rbtree_pattern
from gnat_runtime.utils import PrettyPrinter


class BaseSetPrinter(PrettyPrinter):
    """Base class for set pretty-printers."""

    # TODO: GDB has no 'set' display hint yet. Update this once it is the case!

    def children(self):
        for i, node in enumerate(self.get_node_iterator()):
            yield ('[{}]'.format(i), node['element'])

    def to_string(self):
        return '{} of length {}'.format(
            str(self.value.type),
            self.length
        )


class OrderedSetPrinter(BaseSetPrinter):
    """Pretty-print Ada.Containers.Ordered_Sets.Map values."""

    name            = 'Ordered_Set'
    generic         = 'ada.containers.ordered_sets'
    type_tag_suffix = 'set'

    type_pattern    = Match.TypeName(suffix='__set', pattern=Match.Struct(
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


class HashedSetPrinter(BaseSetPrinter):
    """Pretty-print Ada.Containers.Hashed_Sets.Map values."""

    name            = 'Hashed_Set'
    generic         = 'ada.containers.hashed_sets'
    type_tag_suffix = 'set'

    type_pattern    = Match.TypeName(suffix='__set', pattern=Match.Struct(
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
