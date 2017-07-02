import gdb


gdb_code_names = {
    getattr(gdb, name): name
    for name in dir(gdb)
    if name.startswith('TYPE_CODE_')
}


system_address = gdb.lookup_type('system__address')


def ptr_to_int(ptr_value):
    """
    Convert an access GDB value into the corresponding Python integer.
    """
    return int(ptr_value.cast(system_address))


def strip_typedefs(value):
    return value.cast(value.type.strip_typedefs())


def strip_type_name_suffix(type_name):
    """
    Strip any type name suffix.

    GNAT can add a suffix for body-nested package entities or for protected
    objects.  This is completely irrelevant for our name matchers, so use this
    helper to strip these suffixes before comparing type names.
    """
    if type_name:
        for body_pkg_suffix in ('X', 'Xb', 'Xn', 'V'):
            if type_name.endswith(body_pkg_suffix):
                type_name = type_name[:-len(body_pkg_suffix)]
                break
    return type_name


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


def addr_to_val(addr, valtype):
    """
    Create a value based on the given address and type.

    :param gdb.Value addr: Address to use.
    :param gdb.Type valtype: Type to use.
    :rtype: gdb.Value
    """
    return addr.cast(valtype.pointer()).dereference()
