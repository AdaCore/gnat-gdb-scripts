import gdb

from gnat_runtime.utils import strip_type_name_suffix


class GDBSubprinter(gdb.printing.SubPrettyPrinter):
    def __init__(self, cls, generics):
        self.cls = cls
        self.generics = generics
        super(GDBSubprinter, self).__init__(cls.name)

    def matches(self, val):
        # If possible, try to match the name of `val`'s type.
        type_tag = strip_type_name_suffix(val.type.tag)
        if type_tag is not None:
            # This is for non-generic cases.
            if getattr(self.cls, 'type_tag', None):
                return type_tag == self.cls.type_tag

            # This is for types coming from generics instantiation.
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

        # Otherwise, try to pattern match.
        if hasattr(self.cls, 'type_pattern'):
            return self.cls.type_pattern.match(val.type)

        return False

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
