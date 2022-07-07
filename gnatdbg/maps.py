"""
Pretty-printers for hashed and ordered maps in Ada.Containers and
GNAT.Dynamic_HTables.
"""

from __future__ import annotations

from itertools import count
import re
from typing import Iterator, Tuple

import gdb

from gnatdbg.generics import Match
from gnatdbg.hash_tables import iterate, get_htable_pattern
from gnatdbg.printers import PrettyPrinter
from gnatdbg.red_black_trees import dfs, get_rbtree_pattern
from gnatdbg.utils import coerce_array, iter_array, pretty_typename


class BaseMapPrinter(PrettyPrinter):
    """Base class for map pretty-printers."""

    def display_hint(self) -> str:
        return "map"

    def get_node_iterator(self) -> Iterator[gdb.Value]:
        raise NotImplementedError()

    def length(self) -> int:
        raise NotImplementedError()

    def children(self) -> Iterator[Tuple[str, gdb.Value]]:
        # This is an infinite iterator by design: the exit path is not
        # coverable.
        names = iter("[{}]".format(i) for i in count(0))  # no-code-coverage

        for node in self.get_node_iterator():
            yield (next(names), node["key"])
            yield (next(names), node["element"])

    def to_string(self) -> str:
        return "{} of length {}".format(
            pretty_typename(self.value.type), self.length
        )


class OrderedMapPrinter(BaseMapPrinter):
    """Pretty-print Ada.Containers.Ordered_Maps.Map values."""

    name = "Ordered_Map"

    type_pattern = Match.TypeName(
        suffix=".map",
        pattern=Match.Struct(
            Match.Field("_parent"),
            Match.Field(
                "tree",
                get_rbtree_pattern(
                    Match.Struct(
                        Match.Field("parent", Match.Pointer()),
                        Match.Field("left", Match.Pointer()),
                        Match.Field("right", Match.Pointer()),
                        Match.Field("color", Match.Enum()),
                        Match.Field("key"),
                        Match.Field("element"),
                    )
                ),
            ),
        ),
    )

    @property
    def length(self) -> int:
        return int(self.value["tree"]["length"])

    def get_node_iterator(self) -> Iterator[gdb.Value]:
        return dfs(self.value["tree"])


class OrderedMapCursorPrinter(PrettyPrinter):
    """Pretty-print Ada.Containers.Ordered_Maps.Cursor values."""

    name = "Ordered_Map_Cursor"

    type_pattern = Match.TypeName(
        suffix=".cursor",
        pattern=Match.Struct(
            Match.Field(
                "container", Match.Pointer(OrderedMapPrinter.type_pattern)
            ),
            Match.Field("node", Match.Pointer()),
        ),
    )

    def to_string(self) -> str:
        if self.value["container"]:
            assoc = "{} => {}".format(
                self.value["node"]["key"],
                self.value["node"]["element"],
            )
        else:
            assoc = "No_Element"
        return "Cursor ({})".format(assoc)


class HashedMapPrinter(BaseMapPrinter):
    """Pretty-print Ada.Containers.Hashed_Maps.Map values."""

    name = "Hashed_Map"

    type_pattern = Match.TypeName(
        suffix=".map",
        pattern=Match.Struct(
            Match.Field("_parent"),
            Match.Field(
                "ht",
                get_htable_pattern(
                    Match.Struct(
                        Match.Field("key"),
                        Match.Field("element"),
                        Match.Field("next", Match.Pointer()),
                    )
                ),
            ),
        ),
    )

    @property
    def length(self) -> int:
        return int(self.value["ht"]["length"])

    def get_node_iterator(self) -> Iterator[gdb.Value]:
        return iterate(self.value["ht"])


class HashedMapCursorPrinter(PrettyPrinter):
    """Pretty-print Ada.Containers.Hashed_Maps.Cursor values."""

    name = "Hashed_Map_Cursor"

    type_pattern = Match.TypeName(
        suffix=".cursor",
        pattern=Match.Struct(
            Match.Field(
                "container", Match.Pointer(HashedMapPrinter.type_pattern)
            ),
            Match.Field("node", Match.Pointer()),
            Match.Field(
                "position", Match.TypeName(name="ada.containers.hash_type")
            ),
        ),
    )

    def to_string(self) -> str:
        if self.value["container"]:
            assoc = "{} => {}".format(
                self.value["node"]["key"],
                self.value["node"]["element"],
            )
        else:
            assoc = "No_Element"
        return "Cursor ({})".format(assoc)


class SimpleHTablePrinter(PrettyPrinter):
    """
    Pretty-print GNAT.Dynamic_HTables.Simple_HTable values.

    It is the same as the pretty-printer for Ada.Container.Hashed_Maps.Map
    except the type_pattern, representation of uninitialised map, lack of
    "length" and "table/k/e" instead of "buckets/key/element".
    """

    name = "Simple_HTable"

    def display_hint(self) -> str:
        return "map"

    def get_node_iterator(self) -> Iterator[gdb.Value]:
        if not self.value:
            return

        buckets = coerce_array(self.value["table"])
        assert buckets is not None

        for node in iter_array(buckets):
            while node:
                yield node
                node = node["next"]

    def children(self) -> Iterator[Tuple[str, gdb.Value]]:
        # This is an infinite iterator by design: the exit path is not
        # coverable.
        names = iter("[{}]".format(i) for i in count(0))  # no-code-coverage

        for node in self.get_node_iterator():
            yield (next(names), node["k"])
            yield (next(names), node["e"])

    def to_string(self) -> str:
        # Strip the "access ..." prefix
        # Replace the ".tag.instance_data" suffix with ".instance"
        return re.sub(
            r"\.tab\.instance_data$",
            ".instance",
            str(self.value.type.target()),
        ) + ("" if self.value else " is nil")

    type_pattern = Match.TypeName(
        suffix=".tab.instance_data",
        pattern=Match.Pointer(
            Match.Struct(
                Match.Field(
                    "table",
                    Match.Array(element=Match.Typedef(Match.Pointer())),
                ),
                Match.Field("iterator_index", Match.Integer()),
                Match.Field("iterator_ptr", Match.Pointer()),
                Match.Field("iterator_started", Match.Bool()),
            ),
        ),
    )
