from gnatdbg.tagged import *

c = gdb.parse_and_eval('c')
inf = gdb.selected_inferior()
system_address = get_system_address()

# Replace C's tag with our copy
tag_addr = get_tag_addr(c)
tag_copy_raw_addr = inf.read_memory(
    gdb.parse_and_eval('corrupted_tag_addr').address,
    system_address.sizeof
)
inf.write_memory(tag_addr.address, tag_copy_raw_addr)

# Check that our tag copy is valid, and thus that the test means something
print('Valid tag:')
print('  ' + str(reinterpret_tagged(c)))

# Now corrupt the tag copy and check that gnatdbg.tagged properly detects it
print('Invalid tag:')
tag_copy_addr = gdb.parse_and_eval('corrupted_tag').address
inf.write_memory(tag_copy_addr, 'a' * system_address.sizeof)
try:
    print('  ' + str(reinterpret_tagged(c)))
except gdb.MemoryError as exc:
    print('  {}: {}'.format(type(exc).__name__, exc))
