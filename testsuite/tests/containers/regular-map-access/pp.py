from gnatdbg.printers import GDBPrettyPrinters, PrettyPrinter
from gnatdbg.strings import StringAccess
from gdb.printing import register_pretty_printer


class StringAccessPrinter(PrettyPrinter):
    name = "main.name_access"
    type_pretty_name = "main.name_access"

    def to_string(self):
        value = self.value.dereference()["value"]
        return f"Name({value})"


printers = GDBPrettyPrinters("custom")
printers.append(StringAccessPrinter)
register_pretty_printer(None, printers)
