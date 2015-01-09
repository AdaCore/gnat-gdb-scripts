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
