class PrettyPrinter(object):
    """Base class for pretty-printers."""

    def __init__(self, value):
        self.value = value


class UnboundedStringPrinter(PrettyPrinter):
    """Pretty-print Ada.Strings.Unbounded.Unbounded_String values."""

    name     = 'Unbounded_String'
    type_tag = 'ada__strings__unbounded__unbounded_string'

    def to_string(self):
        # Currently, it seems that the type associated to variable-length
        # arrays in discriminated records have incorrect bounds when accessing
        # the corresponding fields from a value.  This is why we compute
        # manually bounds here and cast the value before returning.
        unb_str = self.value['reference']
        data = unb_str['data']

        lower_bound = 1
        upper_bound = int(unb_str['last'])
        # GDB's Python API requires array length (U - L + 1) not to be
        # negative.
        if lower_bound > upper_bound:
            upper_bound = lower_bound - 1

        nested_type = data.type.target()
        array_type = nested_type.array(lower_bound, upper_bound)
        data_str = str(data.cast(array_type))

        return '{} ({})'.format(self.name, data_str)


class VectorPrinter(PrettyPrinter):
    """Pretty-print Ada.Containers.Vectors.Vector values."""

    name            = 'Vector'
    generic         = 'ada.containers.vectors'
    type_tag_suffix = 'vector'

    def display_hint(self):
        return 'array'

    @property
    def array_bounds(self):
        elements = self.value['elements']
        if not elements:
            return (1, 0)
        array_type = elements['ea'].type
        first_index, _ = array_type.range()
        last_index = int(self.value['elements']['last'])
        return (first_index, last_index)

    @property
    def length(self):
        first, last = self.array_bounds
        if first <= last:
            return last - first + 1
        else:
            return 0

    @property
    def array_elements(self):
        first, last = self.array_bounds
        if first <= last:
            base_value = self.value['elements']['ea']
            return base_value.cast(
                base_value.type.target().array(first, last)
            )
        else:
            return None

    def children(self):
        elements = self.array_elements
        if elements:
            first_index, last_index = elements.type.range()
            for i in range(first_index, last_index + 1):
                yield ('[{}]'.format(i), elements[i])

    def to_string(self):
        return '{} of length {}'.format(
            str(self.value.type),
            self.length,
        )


class GDBSubprinter(gdb.printing.SubPrettyPrinter):
    def __init__(self, cls):
        self.cls = cls
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
                generic = generics_command.get_generic(package_name)
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
    def __init__(self, name):
        super(GDBPrettyPrinters, self).__init__(name, [])

    def append(self, pretty_printer_cls):
        self.subprinters.append(GDBSubprinter(pretty_printer_cls))

    def __call__(self, val):
        for printer in self.subprinters:
            if printer.matches(val):
                return printer.instantiate(val)
        return None


class GenericsCommand(gdb.Command):
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


generics_command = GenericsCommand()


global_printer = GDBPrettyPrinters('gnat-runtime')
global_printer.append(UnboundedStringPrinter)
global_printer.append(VectorPrinter)

def register_printers(objfile):
    objfile.pretty_printers.append(global_printer)


for objfile in gdb.objfiles():
    register_printers(objfile)
