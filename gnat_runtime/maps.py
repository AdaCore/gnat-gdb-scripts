from itertools import count

from gnat_runtime.hash_tables import iterate
from gnat_runtime.red_black_trees import dfs
from gnat_runtime.utils import PrettyPrinter


class BaseMapPrinter(PrettyPrinter):
    """Base class for map pretty-printers."""

    def display_hint(self):
        return 'map'

    def get_node_iterator(self):
        raise NotImplementedError()

    def children(self):
        names = iter('[{}]'.format(i) for i in count(0))
        for node in self.get_node_iterator():
            yield (next(names), node['key'])
            yield (next(names), node['element'])

    def to_string(self):
        return '{} of length {}'.format(
            str(self.value.type),
            self.length
        )


class OrderedMapPrinter(BaseMapPrinter):
    """Pretty-print Ada.Containers.Ordered_Maps.Map values."""

    name            = 'Ordered_Map'
    generic         = 'ada.containers.ordered_maps'
    type_tag_suffix = 'map'

    @property
    def length(self):
        return self.value['tree']['length']

    def get_node_iterator(self):
        return dfs(self.value['tree'])


class HashedMapPrinter(BaseMapPrinter):
    """Pretty-print Ada.Containers.Hashed_Maps.Map values."""

    name            = 'Hashed_Map'
    generic         = 'ada.containers.hashed_maps'
    type_tag_suffix = 'map'

    @property
    def length(self):
        return self.value['ht']['length']

    def get_node_iterator(self):
        return iterate(self.value['ht'])
