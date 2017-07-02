import gdb

from gnatdbg.generics import Match
from gnatdbg.printers import PrettyPrinter
from gnatdbg.utils import ada_string_repr


class UnboundedString(object):
    """
    Helper class to inspect Ada.Strings.Unbounded.Unbounded_String values.
    """

    def __init__(self, value):
        self.value = value

    @property
    def length(self):
        """
        Return the length of this Unbounded_String.

        :rtype: int
        """
        unb_str = self.value['reference']
        return int(unb_str['last'])

    def get_string(self, encoding=None, errors=None):
        """
        Return the string associated to this Unbounded_String.

        :rtype: str
        """
        unb_str = self.value['reference']
        if self.length <= 0:
            return ''

        data = unb_str['data']

        ptr_to_elt_type = data.type.target().pointer()
        ptr_to_first = data.address.cast(ptr_to_elt_type)

        kwargs = {'length': self.length}
        if encoding:
            kwargs['encoding'] = encoding
        if errors:
            kwargs['errors'] = errors
        return ptr_to_first.string(**kwargs)


class UnboundedStringPrinter(PrettyPrinter):
    """Pretty-print Ada.Strings.Unbounded.Unbounded_String values."""

    name     = 'Unbounded_String'
    type_tag = 'ada__strings__unbounded__unbounded_string'

    def to_string(self):
        val = UnboundedString(self.value)
        try:
            # Latin-1 will enable us to read random binary data as bytes: we
            # will display non-printable ASCII bytes later.
            data_str = val.get_string('latin-1')
        except gdb.MemoryError:
            str_repr = '[Invalid]'
        else:
            str_repr = ada_string_repr(data_str)

        return '{} ({})'.format(self.name, str_repr)


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
