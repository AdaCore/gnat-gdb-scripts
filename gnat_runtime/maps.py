from itertools import count

from gnat_runtime.red_black_trees import dfs
from gnat_runtime.utils import PrettyPrinter


class OrderedMapPrinter(PrettyPrinter):
    """Pretty-print Ada.Containers.Ordered_Maps.Map values."""

    name            = 'Ordered_Map'
    generic         = 'ada.containers.ordered_maps'
    type_tag_suffix = 'map'

    def display_hint(self):
        return 'map'

    @property
    def length(self):
        return self.value['tree']['length']

    def children(self):
        names = iter('[{}]'.format(i) for i in count(0))
        for node in dfs(self.value['tree']):
            yield (next(names), node['key'])
            yield (next(names), node['element'])

    def to_string(self):
        return '{} of length {}'.format(
            str(self.value.type),
            self.length
        )
