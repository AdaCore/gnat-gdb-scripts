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

    setup_done = True

    # TODO: properly integrate with GDB's auto-load facility instead of
    # "brutaly" registersing our printers everywhere.
    for progspace in gdb.progspaces():
        progspace.pretty_printers.append(printers)
