from support.build import gnatmake
from support.gdb import GDBSession


gnatmake('show_hashed_map')
gdb = GDBSession('show_hashed_map')
gdb.run_to(gdb.find_loc('show_hashed_map.adb', 'BREAK'))
gdb.print_expr('m', 'show_hashed_map.integer_hashed_maps.map of length 3 = {["John"] = 40, ["Bob"] = 28, ["Alice"] = 24}')
