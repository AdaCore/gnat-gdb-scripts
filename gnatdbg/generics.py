"""
Helpers to match type patterns in debugged programs.
"""

from contextlib import contextmanager
import re

import gdb

from gnatdbg.utils import gdb_code_names, pretty_typename


regex_type = type(re.compile('.*'))


def type_short_descr(typ):
    """Return a human-readable description of `typ`, a GDB type."""
    attrs = []
    if typ.name:
        attrs.append('name={}'.format(typ.name))
    if typ.code == gdb.TYPE_CODE_STRUCT:
        attrs.append('{} fields'.format(len(typ.fields())))
    return '{} ({})'.format(gdb_code_names[typ.code], ', '.join(attrs))


class MatchTrace(object):
    """Container for type pattern matching traces.

    This holds traceback-like information about the current type pattern
    matching state.
    """

    class Scope(object):
        """Single entry for the traceback-like information.

        This holds two attributes: `pattern`, which is the Match.BasePattern
        instance it represents, and `value`, a string that represents the type
        that this pattern must match.
        """

        def __init__(self, pattern, value):
            self.pattern = pattern
            self.value = value
            self.matched = True

    def __init__(self):
        self.scopes_stack = []
        self.mismatch_stack = None

    @contextmanager
    def scope(self, pattern, actual):
        """
        Context manager to register a Scope for the current matched pattern.
        """
        assert isinstance(actual, (gdb.Type, gdb.Field))
        scope = self.Scope(pattern.short_descr,
                           type_short_descr(actual)
                           if isinstance(actual, gdb.Type) else
                           actual.name)
        self.scopes_stack.append(scope)
        yield
        self.scopes_stack.pop()

    def tag_mismatch(self):
        """Register the current traceback for a type pattern mismatch.

        Return False for convenience.
        """
        if not self.mismatch_stack:
            self.mismatch_stack = list(self.scopes_stack)
        return False

    def check_match(self, has_matched):
        """
        Check that `has_matched` is True. If not, register the current
        traceback for a type pattern mismatch.
        """
        return has_matched or self.tag_mismatch()


class Match(object):
    """
    Namespace whose nested classes help matching type patterns.

    Pretty-printers that work on types defined in generic packages need to know
    which type comes from which generic package intantiation. Nested classes
    provide helpers to match type patterns so that pretty-printers can decide
    wether they support some type just looking at what it looks like.

    For instance, the following will match all structure types that have two
    fields, the first one called "foo" of any time and the second one called
    "bar" of some integer type::

        Match.Struct(
            Match.Field('foo'),
            Match.Field('bar', Match.Integer()),
        )
    """

    class BasePattern(object):
        """Interface for type patterns."""

        def _match(self, typ, mt):
            """Return whether `self` matches a GDB type.

            Return whether `self` matches the `typ` input GDB type. In the
            mismatch case, record a trace in `mt` to ease debugging.

            Subclasses must override this.
            """
            raise NotImplementedError()

        def match(self, value_type, debug=False):
            """Return whether `self` matches the `value_type` GDB type.

            If `debug` is true and the match failed, print a description of the
            chain of checks that led to the mismatch.
            """
            mt = MatchTrace()
            res = self._match(value_type, mt)
            if debug and not res:
                print('Debug: mismatch:')
                for scope in mt.mismatch_stack:
                    print('  {} <-> {}'.format(scope.pattern, scope.value))
            return res

        @property
        def short_descr(self):
            """Return a short string to describe this pattern matcher.

            Helper for the tracking framework.
            """
            return type(self).__name__

    class TypeName(BasePattern):
        """Matches a type based on its name."""

        def __init__(self, pattern=None, name=None, suffix=None,
                     recursive=False, match_pretty_name=True):
            """
            :param Matcher.BasePattern|None pattern: If provided, reject any
                type that does not match `pattern`.

            :param str|re.RegexObject|None name: If it's a string, reject any
                type whose name is different. If it's a regular expression
                object, reject any type whose name isn't matched by it. The
                regular expression must match the whole name.

            :param str|None suffix: If provided, reject any type whose name
                does not end with `suffix`.

            :param bool recursive: If true, consider integer basis types (for
                integer subrange types) or the target type of typedefs for
                matching in addition to the original type.

                This behavior can be surprising: for instance typedef layers
                are used to distinguish unconstrained arrays from accesses to
                these, so this is disabled by default.

            :param bool match_pretty_name: If True, match on the pretty GDB
                type name (`str(gdb_type)`, for instance: "foo.bar"),
                otherwise, use the raw name (`gdb_type.name`, for instance:
                "foo__bar___Xb").
            """
            self.type_pattern = pattern
            self.name = name
            self.suffix = suffix
            self.recursive = recursive
            self.match_pretty_name = match_pretty_name

        @property
        def short_descr(self):
            attrs = []
            if isinstance(self.name, str):
                attrs.append('name={}'.format(self.name))
            elif isinstance(self.name, regex_type):
                attrs.append('name=/{}/'.format(self.name.pattern))
            if self.suffix:
                attrs.append('suffix={}'.format(self.suffix))
            if self.recursive:
                attrs.append('recursive=True')
            return 'TypeName({})'.format(', '.join(attrs))

        def _match(self, typ, mt):
            types = [typ]

            # If recursive, go fetch all types to check for matching
            if self.recursive:
                while typ.code in (gdb.TYPE_CODE_RANGE, gdb.TYPE_CODE_TYPEDEF):
                    typ = typ.target()
                    types.append(typ)

            for typ in types:
                type_name = (pretty_typename(typ)
                             if self.match_pretty_name else typ.name)

                with mt.scope(self, typ):

                    # If a name constraint is given, check it is satisfied
                    if (isinstance(self.name, str) and
                            (not type_name or type_name != self.name)):
                        continue
                    if (isinstance(self.name, regex_type) and
                            (not type_name or not self.name.match(type_name))):
                        continue
                    if self.suffix and not (type_name and
                                            type_name.endswith(self.suffix)):
                        continue

                    # Likewise for the type sub-pattern
                    if (self.type_pattern and
                            not self.type_pattern._match(typ, mt)):
                        continue

                    # If we reach this point, all checks have suceeded: we
                    # found a matching type.
                    return True

            # If we reach this point, no type we analyzed satisfied the given
            # constraints: the original type does not match.
            with mt.scope(self, types[0]):
                return mt.tag_mismatch()

    class Pointer(BasePattern):
        """Matches a pointer type and the pointer type, if provided."""
        def __init__(self, target=None):
            self.target = target

        def _match(self, typ, mt):
            # TODO: types are wrapped in typedefs in a too inconsistent way.
            # For instance, say B is a pointer that is stored inside a
            # structure A; then when B has been known to be a TYPE_CODE_PTR
            # when accessed from a variable of type A while it is a
            # TYPE_CODE_TYPEDEF around a TYPE_CODE_PTR when accessed from a
            # variable that is a structure containing A. So for pointer types,
            # always strip typedefs and be done with it.
            typ = typ.strip_typedefs()
            with mt.scope(self, typ):
                return mt.check_match(
                    typ.code == gdb.TYPE_CODE_PTR and (
                        self.target is None
                        or self.target._match(typ.target(), mt)
                    )
                )

    class Typedef(BasePattern):
        """Matches a typedef and the underlying type, if provided."""
        def __init__(self, target=None):
            self.target = target

        def _match(self, typ, mt):
            with mt.scope(self, typ):
                return mt.check_match(
                    typ.code == gdb.TYPE_CODE_TYPEDEF and (
                        self.target is None
                        or self.target._match(typ.target(), mt)
                    )
                )

    class Integer(BasePattern):
        """Matches an integer/range type, according to its size if asked to."""
        def __init__(self, size=None):
            self.size = size

        @property
        def short_descr(self):
            return 'Integer({})'.format('size={}'.format(self.size)
                                        if self.size else '')

        def _match(self, typ, mt):
            with mt.scope(self, typ):
                return mt.check_match(
                    typ.code in (gdb.TYPE_CODE_INT, gdb.TYPE_CODE_RANGE)
                    and (self.size is None or self.size == typ.sizeof)
                )

    class Enum(BasePattern):
        """Matches all enumeration types."""
        def _match(self, typ, mt):
            with mt.scope(self, typ):
                return typ.code == gdb.TYPE_CODE_ENUM

    class Array(BasePattern):
        """Matches array types and attributes depending on what is provided."""
        def __init__(self, element=None, first=None, last=None):
            self.element = element
            self.first, self.last = first, last

        @property
        def short_descr(self):
            attrs = []
            if self.first is not None:
                attrs.append('first={}'.format(self.first))
            if self.last is not None:
                attrs.append('last={}'.format(self.last))
            return 'Array({})'.format(', '.join(attrs))

        def _match(self, typ, mt):
            with mt.scope(self, typ):
                if not mt.check_match(typ.code == gdb.TYPE_CODE_ARRAY):
                    return False
                if not mt.check_match(not self.element or
                                      self.element._match(typ.target(), mt)):
                    return False

                type_first, type_last = typ.range()
                if not mt.check_match(self.first is None
                                      or self.first == type_first):
                    return False
                if not mt.check_match(self.last is None
                                      or self.last == type_last):
                    return False

                return True

    class Struct(BasePattern):
        """Matches structure types and their fields."""
        def __init__(self, *fields):
            self.fields = fields

        @property
        def short_descr(self):
            return 'Struct({} fields)'.format(len(self.fields))

        def _match(self, typ, mt):
            with mt.scope(self, typ):
                if typ.code != gdb.TYPE_CODE_STRUCT:
                    return mt.tag_mismatch()

                if self.fields:
                    # The compiler can introduce artificial fields into this
                    # structure. We do not want to consider them for matchers.
                    # Luckily, these fields should be the only ones having
                    # upper-case characters in their name.
                    fields = [
                        field
                        for field in typ.fields()
                        if field.name.lower() == field.name
                    ]
                    # ... But we might want to match structures that are
                    # artificial. In such cases, all fields are artificial.
                    if not fields:
                        fields = typ.fields()

                    if len(self.fields) != len(fields):
                        return mt.tag_mismatch()

                    for field_pattern, value_field in zip(
                        self.fields, fields
                    ):
                        if not field_pattern._match(value_field, mt):
                            return mt.tag_mismatch()
                return True

    class Field(BasePattern):
        """Matches a structure field."""
        def __init__(self, name, type=None):
            self.name = name
            self.type = type

        @property
        def short_descr(self):
            return 'Field({})'.format('name={}'.format(self.name)
                                      if self.name else '')

        def _match(self, field, mt):
            with mt.scope(self, field):
                if (self.name is not None
                        and self.name != field.name):
                    return mt.tag_mismatch()

                if self.type and not self.type._match(field.type, mt):
                    return mt.tag_mismatch()
                return True

    class Char(BasePattern):
        """Mathes all character types."""
        def _match(self, typ, mt):
            with mt.scope(self, typ):
                return mt.check_match(typ.code == gdb.TYPE_CODE_CHAR)
