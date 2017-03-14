"""
Helpers to work with GNAT's implementation of red-black tree-based standard
containers.
"""

from gnatdbg.generics import Match


def get_rbtree_pattern(node_pattern):
    """
    Return the type pattern for red-black trees used in GNAT's implementation
    of standard containers for nodes that match the given `node_pattern`.
    """
    node_access_pattern = Match.Pointer(node_pattern)
    return Match.Struct(
        Match.Field('_tag'),
        Match.Field('first',  node_access_pattern),
        Match.Field('last',   node_access_pattern),
        Match.Field('root',   node_access_pattern),
        Match.Field('length', Match.Integer()),
        Match.Field('tc'),
    )


def dfs(tree_value):
    """
    Return an iterator on all nodes in `tree_value`.

    `tree_value` must be a value whose type is Tree_Type from generic
    instantiation of Ada.Containers.Red_Black_Trees.Generic_Tree_Types with
    Node_Type being a record that contains `left` and `right` Node_Access
    fields.
    """

    def traverse_node(node_value):
        if node_value['left']:
            for node in traverse_node(node_value['left']):
                yield node
        yield node_value
        if node_value['right']:
            for node in traverse_node(node_value['right']):
                yield node

    if tree_value['root']:
        for node in traverse_node(tree_value['root']):
            yield node
