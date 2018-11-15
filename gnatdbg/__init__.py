import gdb
import gdb.printing


from gnatdbg.lists import (
    DoublyLinkedListPrinter, DoublyLinkedListCursorPrinter,
)
from gnatdbg.maps import (
    HashedMapPrinter, HashedMapCursorPrinter,
    OrderedMapPrinter, OrderedMapCursorPrinter,
)
from gnatdbg.printers import GDBPrettyPrinters
from gnatdbg.sets import (
    HashedSetPrinter, HashedSetCursorPrinter,
    OrderedSetPrinter, OrderedSetCursorPrinter,
)
from gnatdbg.strings import StringAccessPrinter, UnboundedStringPrinter
from gnatdbg.vectors import VectorPrinter, VectorCursorPrinter
from gnatdbg.utils import register_pretty_printers


_setup_done = False
_printers = None


def create_printers(name='gnat-runtime'):
    """
    Instantiate GDBPrettyPrinters with the given name and register all
    pretty-printers for the GNAT runtime in it. Return this instance.
    """
    printers = GDBPrettyPrinters(name)

    for printer in [
        DoublyLinkedListPrinter,
        DoublyLinkedListCursorPrinter,

        HashedMapPrinter,
        HashedMapCursorPrinter,
        HashedSetPrinter,
        HashedSetCursorPrinter,

        OrderedMapPrinter,
        OrderedMapCursorPrinter,
        OrderedSetPrinter,
        OrderedSetCursorPrinter,

        VectorPrinter,
        VectorCursorPrinter,

        UnboundedStringPrinter,
        StringAccessPrinter,
    ]:
        printers.append(printer)

    return printers


def setup(name='gnat-runtime', globally=True):
    """
    Instantiate pretty-printers for the GNAT runtime with the given name and
    register them.

    If `globally` is true, just register the pretty-printers globally. This is
    the most simple way to make them work.

    If `globally is false, only register them in objfiles that define the
    'adainit' symbol. On one hande, this will not make them work on shared
    objects, but on the other hand this will avoid GDB the cost of looking them
    up in objfiles that don't contain Ada. You can see this as an unsound
    optimization.

    This function does its work the first time it is called, but then does
    nothing if it's called afterwards.
    """
    global _setup_done
    global _printers
    if _setup_done:
        return

    _printers = create_printers(name)

    if globally:
        gdb.printing.register_pretty_printer(None, _printers)

    else:
        # Registers our printers only for objfiles that are Ada main entry
        # points.
        def objfile_filter(objfile):
            adainit = gdb.lookup_global_symbol('adainit' or '_adainit')
            return adainit is not None and adainit.symtab.objfile == objfile

        register_pretty_printers(_printers, objfile_filter)

    _setup_done = True
