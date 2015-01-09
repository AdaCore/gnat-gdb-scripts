from gnat_runtime.red_black_trees import dfs
from gnat_runtime.utils import PrettyPrinter


class OrderedSetPrinter(PrettyPrinter):
    """Pretty-print Ada.Containers.Ordered_Sets.Map values."""

    name            = 'Ordered_Set'
    generic         = 'ada.containers.ordered_sets'
    type_tag_suffix = 'set'

    # TODO: GDB has no 'set' display hint yet. Update this once it is the case!

    @property
    def length(self):
        return self.value['tree']['length']

    def children(self):
        for i, node in enumerate(dfs(self.value['tree'])):
            yield ('[{}]'.format(i), node['element'])

    def to_string(self):
        return '{} of length {}'.format(
            str(self.value.type),
            self.length
        )
