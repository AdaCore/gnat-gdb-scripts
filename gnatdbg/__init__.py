import gdb


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


setup_done = False
printers = None


def setup():
    global setup_done
    global printers
    if setup_done:
        return

    printers = GDBPrettyPrinters('gnat-runtime')

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

    # Registers our printers only for objfiles that are Ada main entry points.
    def objfile_filter(objfile):
        adainit = gdb.lookup_global_symbol('adainit' or '_adainit')
        return adainit is not None and adainit.symtab.objfile == objfile
    register_pretty_printers(printers, objfile_filter)

    setup_done = True
