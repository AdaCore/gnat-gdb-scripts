from gnatdbg.debug import print_type_tree
from gnatdbg.utils import coerce_array


def run(var):
    print('== {} =='.format(var))
    value = gdb.parse_and_eval(var)
    try:
        value = coerce_array(value)
    except ValueError as exc:
        value = '<ValueError: {}>'.format(exc)
    else:
        if value is not None:
            print_type_tree(value.type)
        value = str(value)
    print(value)
