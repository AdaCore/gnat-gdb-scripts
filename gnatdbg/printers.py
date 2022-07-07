"""
Base classes to share code for the pretty-printers defined in gnatdbg.
"""

from __future__ import annotations

from typing import Optional, TYPE_CHECKING, Type

import gdb


if TYPE_CHECKING:
    import gnatdbg.generics


class PrettyPrinter:
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

    name: str
    """
    Human-readable string to describe this pretty-printer.

    Subclasses must override this attribute.
    """

    type_pretty_name: Optional[str] = None
    """
    If not None, this must be a string that describes the exact name
    (gdb.Type.__str__) of types that this pretty-printer must match.

    For non-generic types.
    """

    type_tag: Optional[str] = None
    """
    If not None, this must be a string that describe the exact tag (see
    gdb.Type.tag) of types that this pretty-printer must match.

    For non-generic types.
    """

    type_pattern: Optional[gnatdbg.generics.Match.BasePattern] = None
    """
    If not None, this must be an instance of gnatdbg.generics.Match.BasePattern
    subclasses, to describe the type pattern to match.

    For generic types.
    """

    def __init__(self, value: gdb.Value):
        self.value = value

    def to_string(self) -> str | gdb.LazyString:
        raise NotImplementedError()


class GDBPrettyPrinters(gdb.printing.PrettyPrinter):
    """Holder for all GNAT pretty printers."""

    def __init__(self, name: str):
        super().__init__(name, [])

    def append(self, pretty_printer_cls: Type[PrettyPrinter]) -> None:
        """
        Add an instance of GDBSubprinter for the given `pretty_printer_cls`.
        """
        self.subprinters.append(GDBSubprinter(pretty_printer_cls))

    def __call__(self, val: gdb.Value) -> Optional[PrettyPrinter]:
        """
        If there is one enabled pretty-printer that matches `val`, return an
        instance of PrettyPrinter tied to this value. Return None
        otherwise.
        """
        # "gdb.current_language" was added for this code, but doesn't
        # exist in all versions of gdb yet, so check for it before
        # calling.  The goal here is to only allow these printers to
        # trigger when in Ada mode.
        if (
            hasattr(gdb, "current_language")
            and gdb.current_language() != "ada"
        ):
            return None
        for printer in self.subprinters:
            assert isinstance(printer, GDBSubprinter)
            if printer.enabled and printer.matches(val):
                return printer.instantiate(val)
        return None


class GDBSubprinter(gdb.printing.SubPrettyPrinter):
    """Holder for PrettyPrinter subclasses."""

    def __init__(self, cls: Type[PrettyPrinter]):
        self.cls = cls
        super().__init__(cls.name)

    def matches(self, val: gdb.Value) -> bool:
        """Return whether this pretty-printer matches `val`, a GDB value."""
        # For details about the matching features, see PrettyPrinter's class
        # docstring.
        return (
            (
                bool(self.cls.type_pretty_name)
                and self.cls.type_pretty_name == str(val.type)
            )
            or (bool(self.cls.type_tag) and self.cls.type_tag == val.type.tag)
            or (
                self.cls.type_pattern is not None
                and self.cls.type_pattern.match(val.type)
            )
        )

    def instantiate(self, val: gdb.Value) -> PrettyPrinter:
        return self.cls(val)
