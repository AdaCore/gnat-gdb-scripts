"""
Helpers to ease the discovery/debugging of GDB's Python API.
"""

import itertools

import gdb

from gnatdbg.utils import gdb_code_names


def print_type_tree(typeobj):
    """
    Print a GDB type as a tree on the standard output.

    :param gdb.Type typeobj: Root type for the tree to print.
    """
    counter = iter(itertools.count(0))
    visited = {}

    def helper(t, indent=1):
        indent_str = '  ' * indent
        code_name = gdb_code_names[t.code]

        # To avoid too verbose outputs, try to avoid describing types more than
        # once. As we can't rely on identity of gdb.Type instances, we use
        # instead the following tuple.
        key = (code_name, str(t), t.name)
        try:
            no = visited[key]
        except KeyError:
            no = next(counter)
            visited[key] = no
        else:
            return '%{} ({} : {})'.format(no, t.name, code_name)

        result = '%{} ({} : {})'.format(no, t.name, code_name)
        if t.code in (gdb.TYPE_CODE_PTR, gdb.TYPE_CODE_TYPEDEF):
            result += ':\n{}{}'.format(
                indent_str, helper(t.target(), indent + 1)
            )
        elif t.code == gdb.TYPE_CODE_INT:
            result += ' ({} bytes)'.format(t.sizeof)
        elif t.code == gdb.TYPE_CODE_ARRAY:
            first, last = t.range()
            result += '[{} .. {}]:\n{}{}'.format(
                first, last,
                indent_str, helper(t.target(), indent + 1)
            )
        elif t.code in (gdb.TYPE_CODE_STRUCT, gdb.TYPE_CODE_UNION):
            result += ':'
            for field in t.fields():
                result += '\n{}{}: {}'.format(
                    indent_str, field.name,
                    helper(field.type, indent + 1)
                )
        return result

    print(helper(typeobj))


class PrintGDBTypeTreeCommand(gdb.Command):
    """
    Prints a GDB type as a tree.

    This command is intended to help pretty-printers development. It prints the
    GDB view of user types, i.e. trees of gdb.Type instances.
    """

    def __init__(self):
        super(PrintGDBTypeTreeCommand, self).__init__(
            'dbgtype', gdb.COMMAND_NONE, gdb.COMPLETE_SYMBOL
        )

    def invoke(self, arg, from_tty):
        argv = gdb.string_to_argv(arg)
        typeobj = gdb.parse_and_eval(argv[0]).type
        print_type_tree(typeobj)
