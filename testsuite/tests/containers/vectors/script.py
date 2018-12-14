from gnatdbg.vectors import VectorPrinter

v = gdb.parse_and_eval('v')
oom_vector = gdb.parse_and_eval('{foo.int_vectors.vector}0')

def check_print(label, expr):
    print(label)
    try:
        image = str(expr())
    except gdb.MemoryError as exc:
        print('   gdb.MemoryError: {}'.format(exc))
    else:
        print('   {}'.format(image))

# Check that out-of-bounds is properly reported
check_print('Trying to access element 4 of "V"...',
            lambda: VectorPrinter(v).element(4))

# See how pretty-printing works with a severe memory corruption
check_print('Trying to print vector at address 0...',
            lambda: oom_vector)

# Corrupt the vector and the cursor
gdb.execute('set variable cur.container := 1')
gdb.execute('set variable cur.index := 2')
check_print('Trying to print corrupted cursor...',
            lambda: gdb.parse_and_eval('cur'))
