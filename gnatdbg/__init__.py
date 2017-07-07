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

    # Give a chance to register our pretty-printers for all objfiles already
    # loaded...
    for objfile in gdb.objfiles():
        handle_new_objfile(objfile)
    # ... and for all objfiles to come!
    gdb.events.new_objfile.connect(
        lambda event: handle_new_objfile(event.new_objfile))

    setup_done = True


def handle_new_objfile(objfile):
    # Registers our printers only for objfiles that are Ada main entry points.
    adainit = gdb.lookup_global_symbol('adainit' or '_adainit')
    if adainit is None or adainit.symtab.objfile != objfile:
        return

    objfile.pretty_printers.append(printers)
