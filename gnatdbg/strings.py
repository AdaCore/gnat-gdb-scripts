"""
Pretty-printers and helpers for strings, string accesses and unbounded strings.
"""

from gnatdbg.generics import Match
from gnatdbg.printers import PrettyPrinter


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
        unb_str = self.value["reference"]
        return int(unb_str["last"])

    def get_string(self):
        """
        Return the string associated to this Unbounded_String.

        :rtype: str
        """
        return self.value["reference"]["data"].lazy_string(length=self.length)


class UnboundedStringPrinter(PrettyPrinter):
    """Pretty-print Ada.Strings.Unbounded.Unbounded_String values."""

    name = "Unbounded_String"
    type_pretty_name = "ada.strings.unbounded.unbounded_string"

    def to_string(self):
        val = UnboundedString(self.value)
        return val.get_string()

    def display_hint(self):
        return "string"


class StringAccess(object):
    """
    Helper class to inspect String accesses.
    """

    type_pattern = Match.Typedef(
        Match.Struct(
            Match.Field(
                "P_ARRAY",
                Match.Pointer(
                    Match.Array(
                        element=Match.Char(),
                    )
                ),
            ),
            Match.Field("P_BOUNDS"),
        )
    )

    def __init__(self, value):
        if not self.matches(value):
            raise TypeError("Input is not an access to string")
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
        struct = self.value["P_BOUNDS"]
        return (int(struct["LB0"]), int(struct["UB0"]))

    @property
    def length(self):
        lb, ub = self.bounds
        return 0 if lb > ub else (ub - lb + 1)

    def _ptr(self):
        """Return the string pointer for this value."""
        access = self.value["P_ARRAY"]
        ptr_to_elt_type = access.type.target().target().pointer()
        return access.cast(ptr_to_elt_type)

    @property
    def is_valid(self):
        """Return True if the string is valid (has non-zero address)."""
        return self._ptr() != 0

    def get_string(self):
        """
        Return the accessed string.

        :rtype: str
        """
        return self._ptr().lazy_string(length=self.length)


class StringAccessPrinter(PrettyPrinter):
    """Pretty-print String access values."""

    name = "access String"
    type_pattern = StringAccess.type_pattern

    def __init__(self, value):
        super().__init__(value)
        self.access = StringAccess(value)

    def to_string(self):
        if self.access.is_valid:
            return self.access.get_string()
        return "0x0 <null string access>"

    def display_hint(self):
        if self.access.is_valid:
            return "string"
        return None
