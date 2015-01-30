import gdb


class PrettyPrinter(object):
    """Base class for pretty-printers."""

    def __init__(self, value):
        self.value = value


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


def iter_array(array_value):
    """Return an iterator that yields all elements in `array_value`."""
    array_value = strip_typedefs(array_value)

    # Note: as long as we do not have DWARF/GDB proper support for
    # unconstrained array, we have to deal with fat pointers manually.
    if (
        array_value.type.code == gdb.TYPE_CODE_STRUCT
        and set(field.name for field in array_value.type.fields()) == {
            'P_ARRAY', 'P_BOUNDS'
        }
    ):
        # Get the bounds.
        template = array_value['P_BOUNDS']
        first_index = int(template['LB0'])
        last_index = int(template['UB0'])
        if last_index < first_index:
            last_index = first_index - 1

        # Then get and cast the array to the proper type.
        array_ptr = array_value['P_ARRAY']
        # Peel the pointer, typedef and array layer to rebuild an array type
        # from the element type.
        elt_type = array_ptr.type.target().target().target()
        array_type = elt_type.array(first_index, last_index)
        array_value = array_ptr.cast(array_type.pointer()).dereference()

    if array_value.type.code != gdb.TYPE_CODE_ARRAY:
        raise ValueError('Invalid value type: was expecting an array')

    first_index, last_index = array_value.type.range()
    for i in range(first_index, last_index + 1):
        yield array_value[i]
