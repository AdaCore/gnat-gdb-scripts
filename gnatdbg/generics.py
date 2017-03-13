import gdb

from gnatdbg.utils import strip_type_name_suffix


class Match:
    """
    Namespace whose nested classes help matching type patterns.

    Pretty-printers that work on types defined in generic packages need to know
    which type comes from which generic package intantiation. Nested classes
    provide helpers to match type patterns so that pretty-printers can decide
    wether they support some type just looking at what it looks like.
    """

    class BasePattern(object):
        """Interface for type patterns."""
        def match(self, value_type):
            """Return whether `self` matches the input GDB `value_type`."""
            raise NotImplementedError()

    class TypeName(BasePattern):
        """
        Matches a type based on its name.

        Assuming the user provide any of the following, this pattern will
        reject any type:
          - whose name is not exactly `name`;
          - whose name does not end with `suffix`;
          - that does not match `pattern` (a sub-pattern).
        """
        def __init__(self, pattern=None, name=None, suffix=None):
            self.type_pattern = pattern
            self.name = name
            self.suffix = suffix

        def match(self, value_type):
            type_name = strip_type_name_suffix(value_type.name)
            if self.name:
                if not (type_name and type_name == self.name):
                    return False
            if self.suffix:
                if not (type_name and type_name.endswith(self.suffix)):
                    return False
            return not self.type_pattern or self.type_pattern.match(value_type)

    class Pointer(BasePattern):
        """Matches a pointer type and the pointer type, if provided."""
        def __init__(self, target=None):
            self.target = target

        def match(self, value_type):
            # TODO: types are wrapped in typedefs in a too inconsistent way.
            # For instance, say B is a pointed that is stored inside a
            # structure A; then when B has been known to be a TYPE_CODE_PTR
            # when accessed from a variable of type A while it is a
            # TYPE_CODE_TYPEDEF around a TYPE_CODE_PTR when accessed from a
            # variable that is a structure containing A. So for pointer types,
            # always strip typedefs and be done with it.
            value_type = value_type.strip_typedefs()
            return value_type.code == gdb.TYPE_CODE_PTR and (
                self.target is None
                or self.target.match(value_type.target())
            )

    class Typedef(BasePattern):
        """Matches a typedef and the underlying type, if provided."""
        def __init__(self, target=None):
            self.target = target

        def match(self, value_type):
            return value_type.code == gdb.TYPE_CODE_TYPEDEF and (
                self.target is None
                or self.target.match(value_type.target())
            )

    class Integer(BasePattern):
        """Matches an integer/range type, according to its size if asked to."""
        def __init__(self, size=None):
            self.size = size

        def match(self, value_type):
            return (
                value_type.code in (gdb.TYPE_CODE_INT, gdb.TYPE_CODE_RANGE)
                and (self.size is None or self.size == value_type.sizeof)
            )

    class Enum(BasePattern):
        """Matches all enumeration types."""
        def match(self, value_type):
            return value_type.code == gdb.TYPE_CODE_ENUM

    class Array(BasePattern):
        """Matches array types and attributes depending on what is provided."""
        def __init__(self, element=None, first=None, last=None):
            self.element = element
            self.first, self.last = first, last

        def match(self, value_type):
            if value_type.code != gdb.TYPE_CODE_ARRAY:
                return False
            if self.element and not self.element.match(value_type.target()):
                return False

            type_first, type_last = value_type.range()
            if self.first is not None and self.first != type_first:
                return False
            if self.last is not None and self.last != type_last:
                return False

            return True

    class Struct(BasePattern):
        """Matches structure types and their fields."""
        def __init__(self, *fields):
            self.fields = fields

        def match(self, value_type):
            if value_type.code != gdb.TYPE_CODE_STRUCT:
                return False

            if self.fields:
                # The compiler can introduce artificial fields into this
                # structure. We do not want to consider them for matchers.
                # Luckily, these fields should be the only ones having
                # upper-case characters in their name.
                value_fields = [
                    field
                    for field in value_type.fields()
                    if field.name.lower() == field.name
                ]
                # ... But we might want to match structures that are
                # artificial. In such cases, all fields are artificial.
                if not value_fields:
                    value_fields = value_type.fields()

                if len(self.fields) != len(value_fields):
                    return False
                for field_pattern, value_field in zip(
                    self.fields, value_fields
                ):
                    if not field_pattern.match(value_field):
                        return False
            return True

    class Field(BasePattern):
        """Matches a structure field."""
        def __init__(self, name, type=None):
            self.name = name
            self.type = type

        def match(self, value_field):
            if (self.name is not None
                and self.name != value_field.name
            ):
                return False

            if self.type and not self.type.match(value_field.type):
                return False
            return True

    class Char(BasePattern):
        """Mathes all character types."""
        def match(self, typ):
            return typ.code == gdb.TYPE_CODE_CHAR


class GenericsCommand(gdb.Command):
    """Manages manual detection for Ada generic instantiations.

Pretty-printers that work on types defined in generic packages need to know
which type comes from which generic package. This command provides a way to
associate symbol prefixes to generic packages.

Usage: generics [list]
         Print a list of associations: symbol prefix -> generic package.
       generics add PREFIX GENERIC
         Associate a symbol PREFIX to a GENERIC package.
         For instance:
           generics add foo__int_vectors Foo.Int_Vectors
       generics remove PREFIX
         Remove the association of a symbol PREFIX."""

    NAME = 'generics'

    def __init__(self):
        super(GenericsCommand, self).__init__(
            self.NAME, gdb.COMMAND_NONE, gdb.COMPLETE_SYMBOL
        )

        # Mapping: symbol prefix -> lower-case fully qualified name for generic
        # instantiation.  For instance, if a__b is the symbol name prefix for
        # all symbols coming from the instantiation of the X.Y package, we have
        # an "a__b" -> "x.y" entry.
        self.instances = {}

    def get_generic(self, prefix):
        return self.instances.get(prefix, None)

    def print_usage(self, from_tty, error_msg=None):
        if error_msg:
            gdb.write('{}: {}\n'.format(self.NAME, error_msg))
        gdb.write(
'''Usage: {} [list]
          {} add PREFIX GENERIC
          {} remove PREFIX
''')

    def do_list(self):
        if self.instances:
            for prefix in sorted(self.instances):
                gdb.write('{}: {}\n'.format(prefix, self.instances[prefix]))
        else:
            gdb.write('No generic instance registered\n')

    def do_add(self, prefix, generic):
        self.instances[prefix] = generic

    def do_remove(self, prefix):
        try:
            self.instances.pop(prefix)
        except KeyError:
            gdb.write('No such prefix: {}\n'.format(prefix))

    def invoke(self, arg, from_tty):
        argv = gdb.string_to_argv(arg)

        if len(argv) == 0:
            self.do_list()
            return

        verb = argv.pop(0)
        if verb == 'list':
            if len(argv) != 0:
                return self.print_usage(from_tty, '"list" takes no argument')
            self.do_list()
        elif verb == 'add':
            if len(argv) != 2:
                return self.print_usage(from_tty, '"add" takes 2 arguments')
            self.do_add(*argv)
        elif verb == 'remove':
            if len(argv) != 1:
                return self.print_usage(from_tty, '"add" takes 1 arguments')
            self.do_remove(*argv)
