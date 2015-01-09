from gnat_runtime.generics import Match
from gnat_runtime.utils import PrettyPrinter


class VectorPrinter(PrettyPrinter):
    """Pretty-print Ada.Containers.Vectors.Vector values."""

    name            = 'Vector'

    generic         = 'ada.containers.vectors'
    type_tag_suffix = 'vector'

    type_pattern    = Match.TypeName(suffix='__vector', pattern=Match.Struct(
        Match.Field('_parent'),
        Match.Field('elements', Match.Pointer(
            Match.Struct(
                Match.Field('last', Match.Integer()),
                Match.Field('ea', Match.Array()),
            )
        )),
        Match.Field('last', Match.Integer()),
        Match.Field('busy', Match.Integer()),
        Match.Field('lock', Match.Integer()),
    ))

    def display_hint(self):
        return 'array'

    @property
    def array_bounds(self):
        elements = self.value['elements']
        if not elements:
            return (1, 0)
        array_type = elements['ea'].type
        first_index, _ = array_type.range()
        last_index = int(self.value['last'])
        return (first_index, last_index)

    @property
    def length(self):
        first, last = self.array_bounds
        if first <= last:
            return last - first + 1
        else:
            return 0

    @property
    def array_elements(self):
        first, last = self.array_bounds
        if first <= last:
            base_value = self.value['elements']['ea']
            return base_value.cast(
                base_value.type.target().array(first, last)
            )
        else:
            return None

    def children(self):
        elements = self.array_elements
        if elements:
            first_index, last_index = elements.type.range()
            for i in range(first_index, last_index + 1):
                yield ('[{}]'.format(i), elements[i])

    def to_string(self):
        return '{} of length {}'.format(
            str(self.value.type),
            self.length,
        )
