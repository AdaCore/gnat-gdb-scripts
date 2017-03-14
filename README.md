GDB scripts for Ada programs built with GNAT
============================================

This repository gathers various GDB Python helpers related to the GNAT runtime.
They are intended to make debugging Ada programs easier. These helpers are
currently only made of pretty-printers, but one could also imagine in the
future frame filters, type printers and other clever uses of the GDB Python
API! See the "Examples" section below for a few showcases.


Install
=======

These GDB helpers are organized into a `gnatdbg` Python package, so in
order to use them one has to install the package somewhere Python can find it.
Note that you will need GDB 7.8 or higher. The easiest way to get this working
is probably the following:

  - Put the `gnatdbg` directory into a specific place `$DIR` and don't move it
    afterwards.
  - Open your GDB configuration file (`$HOME/.gdbinit`)
  - Add the following commands:

        python import sys; sys.path.append("$DIR")
        python import gnatdbg; gnatdbg.setup()

    ... and make sure you replaced `$DIR` in the above commands with the
    directory where you moved `gnatdbg`.

  - Congratulations, you are done!


Examples
========

Here are a few example for the helpers provided here in real situation:

    # This is what happens without the helpers:
    (gdb) print my_vector
    $1 = (elements => 0x69b170, last => 3, busy => 0, lock => 0)

    (gdb) print my_hashed_map
    $2 = (ht => (buckets => 0x69b218, length => 3, busy => 0, lock => 0))

    (gdb) print my_hashed_map_cursor
    $3 = (container => 0x7fffffffb860, node => 0x69b650)

    (gdb) print my_ordered_set
    $4 = (tree => (first => 0x69b080, last => 0x69b700, root => 0x69b610,
    length => 3, busy => 0, lock => 0))


    # This is what happens with the helpers:
    (gdb) p my_vector
    $5 = foo.str_vectors.vector of length 3 = {Unbounded_String ("one"), 
      Unbounded_String ("two"), Unbounded_String ("three")}

    (gdb) p my_hashed_map
    $6 = foo.str_to_int.map of length 3 = {[Unbounded_String ("one")] = 1, 
      [Unbounded_String ("three")] = 3, [Unbounded_String ("two")] = 2}

    (gdb) print my_hashed_map_cursor
    $7 = Cursor (Unbounded_String ("two") => 2)

    (gdb) print my_ordered_set
    $8 = foo.int_sets.set of length 3 = {[0] = 1, [1] = 2, [2] = 3}
