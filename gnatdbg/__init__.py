import gdb


from gnatdbg.generics import GenericsCommand
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
from gnatdbg.unbounded_strings import UnboundedStringPrinter
from gnatdbg.vectors import VectorPrinter, VectorCursorPrinter


setup_done = False
printers = None
generics_command = None


def setup():
    global setup_done
    global printers
    global generics_command
    if setup_done:
        return

    generics_command = GenericsCommand()
    printers = GDBPrettyPrinters('gnat-runtime', generics_command)

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
    adainit = gdb.lookup_global_symbol('adainit')
    if adainit is None or adainit.symtab.objfile != objfile:
        return

    objfile.pretty_printers.append(printers)
