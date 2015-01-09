from gnat_runtime.utils import iter_array


def iterate(htable_value):
    """
    Return an iterator on all nodes in `htable_value`.

    `htable_value` must be a value whose type is Hash_Table_Type from the
    generic instantiation of
    Ada.Containers.Hash_Tables.Generic_Hash_Table_Types with Node_Type being a
    record that contains `element` and `next` fields.
    """

    # TODO: For some reason, the access to unconstrained array in the `buckets`
    # field does not work very well as a boolean. Revisit this once DWARF/GDB
    # do things properly with respect to Ada types.
    if str(htable_value['buckets']) != '0x0':
        # Note: Due to a issue in DWARF/GDB, the `buckets` field appears here
        # as a typedef to a fat pointer, not as a pointer (it is an access to
        # unconstrained array in the sources.
        for node in iter_array(htable_value['buckets']):
            while node:
                yield node
                node = node['next']
