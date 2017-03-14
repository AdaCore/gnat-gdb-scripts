import gdb

from gnatdbg.utils import strip_type_name_suffix


class PrettyPrinter(object):
    """
    Base class for pretty-printers.

    Instances must comply to GDB's Pretty Printing API
    (https://sourceware.org/gdb/onlinedocs/gdb/Pretty-Printing-API.html).

    Class attributes (`type_tag`, `generic`, `type_tag_suffix` and
    `type_pattern`) must be overriden to describe the set of types that this
    pretty-printer can match. If it must match types that have a very specific
    name, `type_tag` must be overriden.

    If there is no specific name, it probably means that the type comes from a
    generic package instantiation. As of today, debuggers do not know anything
    about instantiations. We have two workarounds here:

      * users providing manual lists of generic instances: see
        gnatdbg.GenericsCommand and the `generic` and `type_tag_suffix`
        attributes below;

      * type pattern matching: see the `type_pattern` attribute below.

    In this case, subclasses must override all these attributes.
    """

    name = None
    """
    Human-readable string to describe this pretty-printer.

    Subclasses must override this attribute.
    """

    type_tag = None
    """
    If not None, this must be a string that describe the exact name of types
    that this pretty-printer must match.

    For non-generic types.
    """

    generic         = None
    """
    Name of the Ada generic package that contains the definition of the type
    this pretty-printer must match.

    Obviously, for generic types.
    """

    type_tag_suffix = None
    """
    Suffix for the instance-independent part of the symbol name of the type
    this pretty-printer must match. For instance, if we want to match
    "Some_Type", a type declared in a "Pkg" package that is itself declared in
    the "Project.Gen_Module" generic package, this should be
    "__pkg__some_type".

    For generic types.
    """

    type_pattern = None
    """
    If not None, this must be an instance of gnatdbg.generics.Match.BasePattern
    subclasses, to describe the type pattern to match.

    For generic types.
    """

    def __init__(self, value):
        self.value = value

    def to_string(self):
        raise NotImplementedError()


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
        instance of PrettyPrinter tied to this value. Return None
        otherwise.
        """
        for printer in self.subprinters:
            if printer.enabled and printer.matches(val):
                return printer.instantiate(val)
        return None


class GDBSubprinter(gdb.printing.SubPrettyPrinter):
    """Holder for PrettyPrinter subclasses."""

    def __init__(self, cls, generics):
        self.cls = cls
        self.generics = generics
        super(GDBSubprinter, self).__init__(cls.name)

    def matches(self, val):
        """Return whether this pretty-printer matches `val`, a GDB value."""

        # For details about the matching features, see PrettyPrinter's class
        # docstring.

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
