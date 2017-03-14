import itertools

import gdb


class PrintGDBTypeTreeCommand(gdb.Command):
    """
    Prints a GDB type as a tree.

    This command is intended to help pretty-printers development. It prints the
    GDB view of user types (the gdb.Type API).
    """

    def __init__(self):
        super(PrintGDBTypeTreeCommand, self).__init__(
            'dbgtype', gdb.COMMAND_NONE, gdb.COMPLETE_SYMBOL
        )

        self.code_names = {
            getattr(gdb, name): name
            for name in dir(gdb)
            if 'TYPE_CODE_' in name
        }

    def invoke(self, arg, from_tty):
        argv = gdb.string_to_argv(arg)
        value_type = gdb.parse_and_eval(argv[0]).type
        self.print_type(value_type)

    def print_type(self, value_type):
        counter = iter(itertools.count(0))
        visited = {}

        def helper(t, indent=1):
            indent_str = '  ' * indent
            key = str(t)
            try:
                no = visited[key]
            except KeyError:
                no = next(counter)
                visited[key] = no
            else:
                return '%{} ({})'.format(no, t.name)

            result = '%{} ({}) = {}'.format(no, t.name, self.code_names[t.code])
            if t.code in (gdb.TYPE_CODE_PTR, gdb.TYPE_CODE_TYPEDEF):
                result += ':\n{}{}'.format(
                    indent_str, helper(t.target(), indent+1)
                )
            elif t.code == gdb.TYPE_CODE_INT:
                result += ' ({} bytes)'.format(t.sizeof)
            elif t.code == gdb.TYPE_CODE_ARRAY:
                first, last = t.range()
                result += '[{} .. {}]:\n{}{}'.format(
                    first, last,
                    indent_str, helper(t.target(), indent+1)
                )
            elif t.code == gdb.TYPE_CODE_STRUCT:
                result += ':'
                for field in t.fields():
                    result += '\n{}{}: {}'.format(
                        indent_str, field.name,
                        helper(field.type, indent+1)
                    )
            return result

        gdb.write(helper(value_type))
        gdb.write('\n')
