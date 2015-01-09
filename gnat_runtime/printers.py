import gdb


class GDBSubprinter(gdb.printing.SubPrettyPrinter):
    def __init__(self, cls, generics):
        self.cls = cls
        self.generics = generics
        super(GDBSubprinter, self).__init__(cls.name)

    def matches(self, val):
        type_tag = val.type.tag
        if type_tag is None:
            return False

        if hasattr(self.cls, 'generic'):
            suffix = '__' + self.cls.type_tag_suffix
            if type_tag.endswith(suffix):
                package_name = type_tag[:-len(suffix)]
                # TODO: here, we would like to know from debugging information
                # whether `package_name` is an instantiation of
                # Ada.Containers.Vectors. But we can't right now, so we rely on
                # the user to tell us...
                generic = self.generics.get_generic(package_name)
                return (
                    generic is not None
                    and generic.lower() == self.cls.generic.lower()
                )
            else:
                return False
        else:
            return type_tag == self.cls.type_tag

    def instantiate(self, val):
        return self.cls(val)


class GDBPrettyPrinters(gdb.printing.PrettyPrinter):
    def __init__(self, name, generics):
        super(GDBPrettyPrinters, self).__init__(name, [])
        self.generics = generics

    def append(self, pretty_printer_cls):
        self.subprinters.append(GDBSubprinter(
            pretty_printer_cls,
            self.generics
        ))

    def __call__(self, val):
        for printer in self.subprinters:
            if printer.enabled and printer.matches(val):
                return printer.instantiate(val)
        return None


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
