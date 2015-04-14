import gdb


from gnat_runtime.generics import GenericsCommand
from gnat_runtime.lists import (
    DoublyLinkedListPrinter, DoublyLinkedListCursorPrinter,
)
from gnat_runtime.maps import (
    HashedMapPrinter, HashedMapCursorPrinter,
    OrderedMapPrinter, OrderedMapCursorPrinter,
)
from gnat_runtime.printers import GDBPrettyPrinters
from gnat_runtime.sets import (
    HashedSetPrinter, HashedSetCursorPrinter,
    OrderedSetPrinter, OrderedSetCursorPrinter,
)
from gnat_runtime.unbounded_strings import UnboundedStringPrinter
from gnat_runtime.vectors import VectorPrinter, VectorCursorPrinter


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

    printers.append(DoublyLinkedListPrinter)
    printers.append(DoublyLinkedListCursorPrinter)

    printers.append(HashedMapPrinter)
    printers.append(HashedMapCursorPrinter)
    printers.append(HashedSetPrinter)
    printers.append(HashedSetCursorPrinter)

    printers.append(OrderedMapPrinter)
    printers.append(OrderedMapCursorPrinter)
    printers.append(OrderedSetPrinter)
    printers.append(OrderedSetCursorPrinter)

    printers.append(VectorPrinter)
    printers.append(VectorCursorPrinter)

    printers.append(UnboundedStringPrinter)

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
