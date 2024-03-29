"""
Pretty-printers and helpers for big numbers (Ada.Numerics.Big_Numbers).
"""

import gdb


from gnatdbg.printers import PrettyPrinter


class BigInteger:
    """
    Helper class to inspect Big_Integer values.
    """

    def __init__(self, value: gdb.Value):
        self.value = value

    @property
    def _bignum_address(self) -> gdb.Value:
        return self.value["value"]["c"]

    @property
    def is_valid(self) -> bool:
        """
        Return whether this big integer is valid.
        """
        return bool(self._bignum_address)

    def get(self) -> int:
        # The data structure used to store big numbers is
        # System.Shared_Bignums.Bignum_Data. Unfortunately, in most cases we
        # don't have debug info for this type, so we have to poke blindly at
        # memory.
        #
        # We consider that big number data is layed out as an array of unsigned
        # 32-bit values.  The first one gives the number N of 32-bit digits
        # that make up this number (24 least significant bits) and whether that
        # number is negative (25th bit). Then the next N unsigned values are
        # the 32-bit digits for the big number (most significant digits first).
        #
        # TODO: it is not clear reading the spec how the first digit is mapped
        # on big-endian systems.
        uint32_t = (
            gdb.selected_frame().architecture().integer_type(32, signed=False)
        )
        data_ptr = self._bignum_address.cast(uint32_t.pointer())

        info = data_ptr.dereference()
        length = info & 0xFFF
        neg = bool(info >> 24)

        array_ptr_type = uint32_t.array(length).pointer()
        data_array = data_ptr.cast(array_ptr_type).dereference()

        result = 0
        for i in range(int(length)):
            result = result << 32 | int(data_array[i + 1])

        if neg:
            result = -result

        return result


class BigReal:
    """
    Helper class to inspect Big_Real values.
    """

    def __init__(self, value: gdb.Value):
        self.value = value
        self.numerator = BigInteger(value["num"])
        self.denominator = BigInteger(value["den"])

    @property
    def is_valid(self) -> bool:
        return self.numerator.is_valid and self.denominator.is_valid

    def get_numerator(self) -> int:
        return self.numerator.get()

    def get_denominator(self) -> int:
        return self.denominator.get()


class BigIntegerPrinter(PrettyPrinter):
    """Pretty-print Big_Integer values."""

    name = "Big_Integer"
    type_pretty_name = "ada.numerics.big_numbers.big_integers.big_integer"

    def to_string(self) -> str:
        val = BigInteger(self.value)
        value = None
        try:
            if val.is_valid:
                value = val.get()
        except gdb.MemoryError:
            value_repr = "[Invalid]"
        else:
            value_repr = "[Uninitialized]" if value is None else str(value)

        return f"{self.name} ({value_repr})"


class BigRealPrinter(PrettyPrinter):
    """Pretty-print Big_Real values."""

    name = "Big_Real"
    type_pretty_name = "ada.numerics.big_numbers.big_reals.big_real"

    def to_string(self) -> str:
        val = BigReal(self.value)
        try:
            value_repr = (
                f"{val.get_numerator()} / {val.get_denominator()}"
                if val.is_valid
                else "[Uninitialized]"
            )
        except gdb.MemoryError:
            value_repr = "[Invalid]"
        return f"{self.name} ({value_repr})"
