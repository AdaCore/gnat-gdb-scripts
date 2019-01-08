"""
Gathering of various helpers that don't deserve their own submodule.
"""

import re

import gdb
import gdb.printing


gdb_code_names = {
    getattr(gdb, name): name
    for name in dir(gdb)
    if name.startswith('TYPE_CODE_')
}
objfile_filter_true = lambda objfile: True # no-code-coverage

bnpe_suffix_re = re.compile('X[bn]*$')


def register_pretty_printers(printers, objfile_filter=objfile_filter_true):
    """
    Register pretty-printers in existing objfiles and register a hook to
    register them in all objfiles that will be loaded.

    :param gdb.printing.PrettyPrinter printers: Set of pretty-printers to
        register.
    :param (gdb.Objfile) -> bool objfile: Function to restrict the set of
        objfiles into which to register pretty-printers. These will be added
        only to objfiles for which this function returns true.
    """

    def register(objfile):
        if objfile_filter(objfile):
            gdb.printing.register_pretty_printer(objfile, printers)

    # Give a chance to register pretty-printers for all objfiles already
    # loaded...
    for objfile in gdb.objfiles():
        register(objfile)

    # ... and for all objfiles to come!
    gdb.events.new_objfile.connect(lambda event: register(event.new_objfile))


def get_system_address():
    return gdb.lookup_type('system__address')


def address_as_offset(addr):
    """
    In order to avoid requiring too much debug information, we use the
    System.Address type (which is unsigned) to decode what is actually a
    System.Storage_Elements.Storage_Offset (which is signed). This does the
    conversion.

    :param gdb.Value addr: Address to decode.
    :rtype: int
    """
    addr_range_size = 2 ** (8 * addr.type.sizeof)
    max_int = addr_range_size / 2 - 1
    int_addr = int(addr)
    if int_addr > max_int:
        return int_addr - addr_range_size
    else:
        return int_addr


def strip_typedefs(value):
    return value.cast(value.type.strip_typedefs())


def coerce_array(array_value):
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

    if (
        array_value.type.code == gdb.TYPE_CODE_STRUCT
        and set(field.name for field in array_value.type.fields()) == {
            'P_ARRAY', 'P_BOUNDS'
        }
    ):
        template = array_value['P_BOUNDS']
        array_ptr = array_value['P_ARRAY']
        if not (template and array_ptr):
            return None

        # Get the bounds
        first_index = int(template['LB0'])
        last_index = int(template['UB0'])
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
        raise ValueError('Invalid array value: {} ({})'.format(
            array_value, gdb_code_names[array_value.type.code]
        ))

    return array_value


def iter_array(array_value):
    """Return an iterator that yields all elements in `array_value`."""
    array_value = coerce_array(array_value)

    first_index, last_index = array_value.type.range()
    for i in range(first_index, last_index + 1):
        yield array_value[i]


def ada_string_repr(string):
    """
    Format `string` as an Ada string literal.
    """
    chars = []
    for c in string:
        if c == '"':
            chars.append('""')
        elif c < ' ' or c > '~':
            chars.append('["{:02x}"]'.format(ord(c)))
        else:
            chars.append(c)
    return '"{}"'.format(''.join(chars))


def encode_name(name):
    """
    Encode a qualified name into a GNAT encoded name.

    For instance: Foo.Bar_Type -> foo__bar_type

    :param str name: Name to encode:
    :rtype: str
    """
    return name.lower().replace('.', '__')


def pretty_typename(typ):
    """
    Return a pretty (Ada-like) name for the given type.

    :param gdb.Type typ: Type whose name is requested.
    :rtype: str
    """
    return strip_bnpe_suffix(str(typ))


def strip_bnpe_suffix(name):
    """
    Strip suffix for body-nested package entities from "name".

    :param str name: Name to strip.
    :rtype: str
    """
    m = bnpe_suffix_re.search(name)
    return name[:m.start()] if m else name


def addr_to_val(addr, valtype):
    """
    Create a value based on the given address and type.

    :param gdb.Value addr: Address to use.
    :param gdb.Type valtype: Type to use.
    :rtype: gdb.Value
    """
    return addr.cast(valtype.pointer()).dereference()
