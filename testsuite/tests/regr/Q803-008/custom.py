import re

from gnatdbg.printers import GDBPrettyPrinters, PrettyPrinter
from gnatdbg.generics import Match
from gnatdbg.utils import register_pretty_printers


class TimePrettyPrinter(PrettyPrinter):
    name = "main.time"
    type_pattern = Match.TypeName(
        name=re.compile('universal_calendar__(T?)time(B?).*'),
        recursive=True, match_pretty_name=False,
    )

    def to_string(self):
        return 'time'


printers = GDBPrettyPrinters('custom')
printers.append(TimePrettyPrinter)
register_pretty_printers(printers)
