"""
Helpers to work with GNAT's implementation of hash table-based standard
containers.
"""

from gnatdbg.generics import Match
from gnatdbg.utils import coerce_array, iter_array


def get_htable_pattern(node_pattern):
    """
    Return the type pattern for hash tables used in GNAT's implementation of
    standard containers for nodes that match the given `node_pattern`.
    """
    # TODO: unfortunately, due to the current state of DWARF/GDB, it is not
    # possible to reach `node_pattern` through hash table's value type.
    return Match.Struct(
        Match.Field('_tag'),
        # See below for the type of `buckets`.
        Match.Field('buckets', Match.Typedef(Match.Struct(
            Match.Field('P_ARRAY',  Match.Pointer()),
            Match.Field('P_BOUNDS', Match.Pointer()),
        ))),
        Match.Field('length', Match.Integer()),
        Match.Field('tc'),
    )


def iterate(htable_value):
    """
    Return an iterator on all nodes in `htable_value`.

    `htable_value` must be a value whose type is Hash_Table_Type from the
    generic instantiation of
    Ada.Containers.Hash_Tables.Generic_Hash_Table_Types with Node_Type being a
    record that contains `element` and `next` fields.
    """
    buckets = coerce_array(htable_value['buckets'])
    if not buckets:
        return

    for node in iter_array(buckets):
        while node:
            yield node
            node = node['next']
