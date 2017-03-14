import gdb

from gnatdbg.utils import strip_type_name_suffix


class GDBPrettyPrinters(gdb.printing.PrettyPrinter):
    """Holder for all GNAT pretty printers.

    Instances are tied to a specific gnatdbg.generics.GenericsCommand
    instance.
    """

    def __init__(self, name, generics):
        super(GDBPrettyPrinters, self).__init__(name, [])
        self.generics = generics

    def append(self, pretty_printer_cls):
        """
        Add an instance of GDBSubprinter for the given `pretty_printer_cls`.
        """
        self.subprinters.append(GDBSubprinter(
            pretty_printer_cls,
            self.generics
        ))

    def __call__(self, val):
        """
        If there is one enabled pretty-printer that matches `val`, return an
        instance of gnatdbg.utils.PrettyPrinter tied to this value. Return None
        otherwise.
        """
        for printer in self.subprinters:
            if printer.enabled and printer.matches(val):
                return printer.instantiate(val)
        return None


class GDBSubprinter(gdb.printing.SubPrettyPrinter):
    """Holder for gnatdbg.utils.PrettyPrinter subclasses."""

    def __init__(self, cls, generics):
        self.cls = cls
        self.generics = generics
        super(GDBSubprinter, self).__init__(cls.name)

    def matches(self, val):
        """Return whether this pretty-printer matches `val`, a GDB value."""

        # For details about the matching features, see
        # gnatdbg.utils.PrettyPrinter's class docstring.

        # If possible, try to match the name of `val`'s type
        type_tag = strip_type_name_suffix(val.type.tag)
        if type_tag is not None:
            # This is for non-generic cases
            if getattr(self.cls, 'type_tag', None):
                return type_tag == self.cls.type_tag

            # This is for types coming from generics instantiation
            elif (
                getattr(self.cls, 'generic', None)
                and getattr(self.cls, 'type_tag_suffix', None)
            ):
                suffix = '__' + self.cls.type_tag_suffix
                if type_tag.endswith(suffix):
                    package_name = type_tag[:-len(suffix)]
                    # TODO: here, we would like to know from debugging
                    # information whether `package_name` is an instantiation of
                    # Ada.Containers.Vectors. But we can't right now, so we
                    # rely on the user to tell us...
                    generic = self.generics.get_generic(package_name)
                    if (
                        generic is not None
                        and generic.lower() == self.cls.generic.lower()
                    ):
                        return True

        # Otherwise, try to pattern match
        if getattr(self.cls, 'type_pattern', None):
            return self.cls.type_pattern.match(val.type)

        return False

    def instantiate(self, val):
        return self.cls(val)
