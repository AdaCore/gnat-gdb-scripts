"""
Pretty-printers for vectors in Ada.Containers.Vectors.
"""

from __future__ import annotations

from typing import Iterable, Optional, Tuple, Union

import gdb

from gnatdbg.generics import Match
from gnatdbg.printers import PrettyPrinter
from gnatdbg.utils import pretty_typename


class VectorPrinter(PrettyPrinter):
    """Pretty-print Ada.Containers.Vectors.Vector values."""

    name = "Vector"

    type_pattern = Match.TypeName(
        suffix=".vector",
        pattern=Match.Struct(
            Match.Field("_parent"),
            Match.Field(
                "elements",
                Match.Pointer(
                    Match.Struct(
                        Match.Field("last", Match.Integer()),
                        Match.Field("ea", Match.Array()),
                    )
                ),
            ),
            Match.Field("last", Match.Integer()),
            Match.Field("tc"),
        ),
    )

    def display_hint(self) -> str:
        return "array"

    @property
    def array_bounds(self) -> Tuple[int, int]:
        elements = self.value["elements"]
        if not elements:
            return (1, 0)
        array_type = elements["ea"].type
        first_index, _ = array_type.range()
        last_index = int(self.value["last"])
        return (first_index, last_index)

    @property
    def length(self) -> int:
        first, last = self.array_bounds
        if first <= last:
            return last - first + 1
        else:
            return 0

    @property
    def array_elements(self) -> Optional[gdb.Value]:
        first, last = self.array_bounds
        if first <= last:
            base_value = self.value["elements"]["ea"]
            return base_value.cast(base_value.type.target().array(first, last))
        else:
            return None

    def element(self, index: Union[int, gdb.Value]) -> gdb.Value:
        idx = int(index)
        first, last = self.array_bounds
        if first <= idx and idx <= last:
            elts = self.array_elements
            assert elts is not None
            return elts[idx]
        else:
            raise gdb.MemoryError(
                "Out of bound vector access ({} not in {} ..  {})".format(
                    index, first, last
                )
            )

    def children(self) -> Iterable[Tuple[str, gdb.Value]]:
        elements = self.array_elements
        if elements:
            first_index, last_index = elements.type.range()
            for i in range(first_index, last_index + 1):
                elt = elements[i]
                elt.fetch_lazy()
                yield ("[{}]".format(i), elt)

    def to_string(self) -> str:
        return "{} of length {}".format(
            pretty_typename(self.value.type), self.length
        )


class VectorCursorPrinter(PrettyPrinter):
    """Pretty-print Ada.Containers.Vectors.Cursor values."""

    name = "Vector_Cursor"

    type_pattern = Match.TypeName(
        suffix=".cursor",
        pattern=Match.Struct(
            Match.Field(
                "container", Match.Pointer(VectorPrinter.type_pattern)
            ),
            Match.Field("index", Match.Integer()),
        ),
    )

    def to_string(self) -> str:
        if self.value["container"]:
            vector = VectorPrinter(self.value["container"])
            index = self.value["index"]
            try:
                element = str(vector.element(index))
            except gdb.MemoryError:
                return "Cursor ([Invalid])"
            assoc = "{} => {}".format(index, element)
        else:
            assoc = "No_Element"
        return "Cursor ({})".format(assoc)
