"""
Base classes to share code for the pretty-printers defined in gnatdbg.
"""

import gdb


class PrettyPrinter(object):
    """
    Base class for pretty-printers.

    Instances must comply to GDB's Pretty Printing API
    (https://sourceware.org/gdb/onlinedocs/gdb/Pretty-Printing-API.html).

    Class attributes (`type_tag`, `generic`, and `type_pattern`) must be
    overriden to describe the set of types that this pretty-printer can match.
    If it must match types that have a very specific name, `type_tag` must be
    overriden.

    If there is no specific name, it probably means that the type comes from a
    generic package instantiation. As of today, debuggers do not know anything
    about instantiations. We have one workaround here: type pattern matching:
    see the `type_pattern` attribute below.

    In this case, subclasses must override all these attributes.
    """

    name = None
    """
    Human-readable string to describe this pretty-printer.

    Subclasses must override this attribute.
    """

    type_pretty_name = None
    """
    If not None, this must be a string that describes the exact name
    (gdb.Type.__str__) of types that this pretty-printer must match.

    For non-generic types.
    """

    type_tag = None
    """
    If not None, this must be a string that describe the exact tag (see
    gdb.Type.tag) of types that this pretty-printer must match.

    For non-generic types.
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
    """Holder for all GNAT pretty printers."""

    def __init__(self, name):
        super(GDBPrettyPrinters, self).__init__(name, [])

    def append(self, pretty_printer_cls):
        """
        Add an instance of GDBSubprinter for the given `pretty_printer_cls`.
        """
        self.subprinters.append(GDBSubprinter(pretty_printer_cls))

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

    def __init__(self, cls):
        self.cls = cls
        super(GDBSubprinter, self).__init__(cls.name)

    def matches(self, val):
        """Return whether this pretty-printer matches `val`, a GDB value."""
        # For details about the matching features, see PrettyPrinter's class
        # docstring.
        return (
            (self.cls.type_pretty_name and
                self.cls.type_pretty_name == str(val.type)) or
            (self.cls.type_tag and self.cls.type_tag == val.type.tag) or
            (self.cls.type_pattern and self.cls.type_pattern.match(val.type))
        )

    def instantiate(self, val):
        return self.cls(val)
