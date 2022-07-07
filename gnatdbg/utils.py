"""
Gathering of various helpers that don't deserve their own submodule.
"""

from __future__ import annotations

import re
from typing import Callable, Iterator, Optional, cast

import gdb
import gdb.printing


gdb_code_names = {
    getattr(gdb, name): cast(int, name)
    for name in dir(gdb)
    if name.startswith("TYPE_CODE_")
}

ObjfileFilter = Callable[[gdb.Objfile], bool]


bnpe_suffix_re = re.compile("X[bn]*$")


def get_system_address() -> gdb.Type:
    return gdb.lookup_type("system__address")


def address_as_offset(addr: gdb.Value) -> int:
    """
    In order to avoid requiring too much debug information, we use the
    System.Address type (which is unsigned) to decode what is actually a
    System.Storage_Elements.Storage_Offset (which is signed). This does the
    conversion.

    :param addr: Address to decode.
    """
    addr_range_size = 2 ** (8 * addr.type.sizeof)
    max_int = addr_range_size / 2 - 1
    int_addr = int(addr)
    if int_addr > max_int:
        return int_addr - addr_range_size
    else:
        return int_addr


def strip_typedefs(value: gdb.Value) -> gdb.Value:
    return value.cast(value.type.strip_typedefs())


def coerce_array(array_value: gdb.Value) -> Optional[gdb.Value]:
    """
    Turn `array_value` into a proper GDB array value.

    As long as we do not have DWARF/GDB proper support for unconstrained array,
    we have to deal with fat pointers manually. If `array_type` is such a fat
    pointer, create an array value with the appropriate bound info. If it is
    actually a null fat pointer, just return None for convenience.

    If `array_value` is already a ready-to-use array value, just return it.

    If it's anything else, raise a ValueError.
    """
    array_value = strip_typedefs(array_value)
    if array_value.type.code == gdb.TYPE_CODE_REF:
        array_value = array_value.referenced_value()
    array_value = strip_typedefs(array_value)

    if array_value.type.code == gdb.TYPE_CODE_STRUCT and {
        field.name for field in array_value.type.fields()
    } == {"P_ARRAY", "P_BOUNDS"}:
        template = array_value["P_BOUNDS"]
        array_ptr = array_value["P_ARRAY"]
        if not (template and array_ptr):
            return None

        # Get the bounds
        first_index = int(template["LB0"])
        last_index = int(template["UB0"])
        if last_index < first_index:
            last_index = first_index - 1

        # Peel the pointer and array type layers
        array_ptr_type = array_ptr.type
        assert array_ptr_type.code == gdb.TYPE_CODE_PTR
        raw_array_type = array_ptr_type.target()
        assert raw_array_type.code == gdb.TYPE_CODE_ARRAY
        elt_type = raw_array_type.target()

        # Rebuild an array type from the bounds + element type
        array_type = elt_type.array(first_index, last_index)
        return array_ptr.cast(array_type.pointer()).dereference()

    if array_value.type.code != gdb.TYPE_CODE_ARRAY:
        raise ValueError(
            "Invalid array value: {} ({})".format(
                array_value, gdb_code_names[array_value.type.code]
            )
        )

    return array_value


def iter_array(array_value: gdb.Value) -> Iterator[gdb.Value]:
    """Return an iterator that yields all elements in `array_value`."""
    v = coerce_array(array_value)
    assert v is not None

    first_index, last_index = v.type.range()
    for i in range(first_index, last_index + 1):
        yield v[i]


def encode_name(name: str) -> str:
    """
    Encode a qualified name into a GNAT encoded name.

    For instance: Foo.Bar_Type -> foo__bar_type

    :param name: Name to encode:
    """
    return name.lower().replace(".", "__")


def pretty_typename(typ: gdb.Type) -> str:
    """
    Return a pretty (Ada-like) name for the given type.

    :param typ: Type whose name is requested.
    """
    return strip_bnpe_suffix(str(typ))


def strip_bnpe_suffix(name: str) -> str:
    """
    Strip suffix for body-nested package entities from "name".

    :param name: Name to strip.
    """
    m = bnpe_suffix_re.search(name)
    return name[: m.start()] if m else name


def addr_to_val(addr: gdb.Value, valtype: gdb.Type) -> gdb.Value:
    """
    Create a value based on the given address and type.

    :param addr: Address to use.
    :param valtype: Type to use.
    """
    return addr.cast(valtype.pointer()).dereference()
