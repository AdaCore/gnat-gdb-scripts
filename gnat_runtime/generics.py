import gdb


class GenericsCommand(gdb.Command):
    """Manages manual detection for Ada generic instantiations.

Pretty-printers that work on types defined in generic packages need to know
what type comes from what generic package. This command provides a way to
associate symbol prefixes to generic packages.

Usage: generics [list]
         Print a list of associations: symbol prefix -> generic package.
       generics add PREFIX GENERIC
         Associate a symbol PREFIX to a GENERIC package.
         For instance:
           generics add foo__int_vectors Foo.Int_Vectors
       generics remove PREFIX
         Remove the association of a symbol PREFIX."""

    NAME = 'generics'

    def __init__(self):
        super(GenericsCommand, self).__init__(
            self.NAME, gdb.COMMAND_NONE, gdb.COMPLETE_SYMBOL
        )

        # Mapping: symbol prefix -> lower-case fully qualified name for generic
        # instantiation.  For instance, if a__b is the symbol name prefix for
        # all symbols coming from the instantiation of the X.Y package, we have
        # an "a__b" -> "x.y" entry.
        self.instances = {}

    def get_generic(self, prefix):
        return self.instances.get(prefix, None)

    def print_usage(self, from_tty, error_msg=None):
        if error_msg:
            gdb.write('{}: {}\n'.format(self.NAME, error_msg))
        gdb.write(
'''Usage: {} [list]
          {} add PREFIX GENERIC
          {} remove PREFIX
''')

    def do_list(self):
        if self.instances:
            for prefix in sorted(self.instances):
                gdb.write('{}: {}\n'.format(prefix, self.instances[prefix]))
        else:
            gdb.write('No generic instance registered\n')

    def do_add(self, prefix, generic):
        self.instances[prefix] = generic

    def do_remove(self, prefix):
        try:
            self.instances.pop(prefix)
        except KeyError:
            gdb.write('No such prefix: {}\n'.format(prefix))

    def invoke(self, arg, from_tty):
        argv = gdb.string_to_argv(arg)

        if len(argv) == 0:
            self.do_list()
            return

        verb = argv.pop(0)
        if verb == 'list':
            if len(argv) != 0:
                return self.print_usage(from_tty, '"list" takes no argument')
            self.do_list()
        elif verb == 'add':
            if len(argv) != 2:
                return self.print_usage(from_tty, '"add" takes 2 arguments')
            self.do_add(*argv)
        elif verb == 'remove':
            if len(argv) != 1:
                return self.print_usage(from_tty, '"add" takes 1 arguments')
            self.do_remove(*argv)
