import gdb


class PrettyPrinter(object):
    """Base class for pretty-printers."""

    def __init__(self, value):
        self.value = value


gdb_code_names = {
    getattr(gdb, name): name
    for name in dir(gdb)
    if name.startswith('TYPE_CODE_')
}


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
    pointer, create an array value with the appropriate bound info.

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
        # Get the bounds
        template = array_value['P_BOUNDS']
        first_index = int(template['LB0'])
        last_index = int(template['UB0'])
        if last_index < first_index:
            last_index = first_index - 1

        # Then get and cast the array to the proper type
        array_ptr = array_value['P_ARRAY']

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
