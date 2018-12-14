"""
Pretty-printers and helpers for strings, string accesses and unbounded strings.
"""

import gdb

from gnatdbg.generics import Match
from gnatdbg.printers import PrettyPrinter
from gnatdbg.utils import ada_string_repr


def _fetch_string(array_access, length, encoding, errors=None):
    """
    Fetch the string value in `array_access`.

    :param gdb.Value array_access: Value that is a pointer to the array that
        represents the string.
    :param int length: Length of the string to fetch.
    :param str encoding: Encoding used to decode the string. Same meaning as in
        str.decode().
    :param str|None errors: Error handling for string decoding. Same meaning
        as in str.decode().
    :rtype: str
    """
    if length <= 0:
        return ''

    ptr_to_elt_type = array_access.type.target().target().pointer()
    ptr_to_first = array_access.cast(ptr_to_elt_type)

    kwargs = {'length': length, 'encoding': encoding}
    if errors:
        kwargs['errors'] = errors
    return ptr_to_first.string(**kwargs)


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
        return _fetch_string(self.value['reference']['data'].address,
                             self.length, encoding, errors)


class UnboundedStringPrinter(PrettyPrinter):
    """Pretty-print Ada.Strings.Unbounded.Unbounded_String values."""

    name             = 'Unbounded_String'
    type_pretty_name = 'ada.strings.unbounded.unbounded_string'

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


class StringAccess(object):
    """
    Helper class to inspect String accesses.
    """

    type_pattern = Match.Typedef(Match.Struct(
        Match.Field('P_ARRAY', Match.Pointer(Match.Array(
            element=Match.Char(),
        ))),
        Match.Field('P_BOUNDS'),
    ))

    default_encodings = {
        1: None,
        2: 'utf-16',
        4: 'utf-32',
    }

    def __init__(self, value):
        if not self.matches(value):
            raise TypeError('Input is not an access to string')
        self.value = value

    @classmethod
    def matches(cls, value):
        """
        Return whether `value` is a string access.

        :param gdb.Value value: Value to test.
        :rtype: bool
        """
        return cls.type_pattern.match(value.type)

    @property
    def bounds(self):
        """
        Return the bounds of the accessed string.

        :rtype: (int, int)
        """
        struct = self.value['P_BOUNDS']
        return (int(struct['LB0']), int(struct['UB0']))

    @property
    def length(self):
        lb, ub = self.bounds
        return 0 if lb > ub else (ub - lb + 1)

    def get_string(self, encoding=None, errors=None):
        """
        Return the accessed string.

        :rtype: str
        """
        return _fetch_string(self.value['P_ARRAY'], self.length, encoding,
                             errors)


class StringAccessPrinter(PrettyPrinter):
    """Pretty-print String access values."""

    name = 'access String'
    type_pattern = StringAccess.type_pattern

    def to_string(self):
        val = StringAccess(self.value)

        # Depending on the size of the character type, determine the string
        # encoding to use to fetch the string. By default, use latin-1 which
        # will enable us to read random binary data as bytes: we will display
        # non-printable ASCII bytes later.
        fatptr_type = self.value.type.strip_typedefs()
        char_type = fatptr_type.fields()[0].type.target().target()
        encoding = (StringAccess.default_encodings.get(char_type.sizeof, None)
                    or 'latin-1')

        try:
            data_str = val.get_string(encoding)
        except gdb.MemoryError:
            str_repr = '[Invalid]'
        else:
            str_repr = ada_string_repr(data_str)

        return '({}) {} {}'.format(
            self.value.type.name,
            self.value['P_ARRAY'],
            str_repr
        )
