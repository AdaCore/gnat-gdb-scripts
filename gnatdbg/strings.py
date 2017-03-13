import gdb

from gnatdbg.generics import Match
from gnatdbg.utils import PrettyPrinter


class UnboundedStringPrinter(PrettyPrinter):
    """Pretty-print Ada.Strings.Unbounded.Unbounded_String values."""

    name     = 'Unbounded_String'
    type_tag = 'ada__strings__unbounded__unbounded_string'

    def to_string(self):
        try:
            data_str = self.get_string_value()
        except gdb.MemoryError:
            data_str = '[Invalid]'

        return '{} ({})'.format(self.name, data_str)

    def get_string_value(self):
        # Currently, it seems that the type associated to variable-length
        # arrays in discriminated records have incorrect bounds when accessing
        # the corresponding fields from a value.  This is why we compute
        # manually bounds here and cast the value before returning.
        unb_str = self.value['reference']
        data = unb_str['data']

        lower_bound = 1
        upper_bound = int(unb_str['last'])
        # GDB's Python API requires array length (U - L + 1) not to be
        # negative.
        if lower_bound > upper_bound:
            upper_bound = lower_bound - 1

        nested_type = data.type.target()
        array_type = nested_type.array(lower_bound, upper_bound)
        return str(data.cast(array_type))


class StringAccessPrinter(PrettyPrinter):
    """Pretty-print String access values."""

    name = 'access String'
    type_pattern = Match.Typedef(Match.Struct(
        Match.Field('P_ARRAY', Match.Pointer(Match.Array(
            element=Match.Char(),
        ))),
        Match.Field('P_BOUNDS'),
    ))

    def to_string(self):
        try:
            data_str = self.get_string_value()
        except gdb.MemoryError:
            data_str = '[Invalid]'

        return '({}) {} {}'.format(
            self.value.type.name,
            self.value['P_ARRAY'],
            str(data_str)
        )

    def get_string_value(self):
        data_ptr = self.value['P_ARRAY']
        bounds = self.value['P_BOUNDS']

        lower_bound = int(bounds['LB0'])
        upper_bound = int(bounds['UB0'])
        # GDB's Python API requires array length (U - L + 1) not to be
        # negative.
        if lower_bound > upper_bound:
            upper_bound = lower_bound - 1

        nested_type = data_ptr.type.target().target()
        array_type = nested_type.array(lower_bound, upper_bound).pointer()
        return str(data_ptr.cast(array_type).dereference())
