import gdb


from gnat_runtime.maps import OrderedMapPrinter
from gnat_runtime.printers import GDBPrettyPrinters, GenericsCommand
from gnat_runtime.unbounded_strings import UnboundedStringPrinter
from gnat_runtime.vectors import VectorPrinter


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

    printers.append(OrderedMapPrinter)
    printers.append(UnboundedStringPrinter)
    printers.append(VectorPrinter)

    setup_done = True

    # TODO: properly integrate with GDB's auto-load facility instead of
    # "brutaly" registersing our printers everywhere.
    for objfile in gdb.objfiles():
        objfile.pretty_printers.append(printers)
