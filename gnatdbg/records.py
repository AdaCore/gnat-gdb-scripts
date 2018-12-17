"""
Helpers to deal with GNAT encondings related to records.
"""

from collections import OrderedDict

import gdb


class DiscriminantMatcher(object):
    """
    Helper to match discriminant values.
    """
    def __init__(self, values, ranges, match_all):
        """
        :param list[int] values: List of values that this matcher accepts.
        :param list[(int, int)] ranges: List of inclusive ranges that this
            matcher accepts.
        :param bool match_all: Whether this matcher must match all discriminant
            values.
        """
        if match_all:
            assert values is None and ranges is None
        else:
            assert values or ranges, '{}, {}'.format(values, ranges)
        self.values = values
        self.ranges = ranges
        self.match_all = match_all

    def __repr__(self):
        if self.match_all:
            content = 'others'
        else:
            content = ' | '.join(
                [str(v) for v in self.values] +
                ['{} .. {}'.format(first, last) for first, last in self.ranges]
            )
        return 'DiscriminantMatcher({})'.format(content)

    @classmethod
    def decode(cls, field_name):
        """
        Parse a GNAT encoding for a discriminant matcher. These correspond to
        "when" clauses.

        :param str field_name: GNAT encoding to parse (this is a union field
            name).
        :rtype: DiscriminantMatcher
        """
        if field_name == 'O':
            return DiscriminantMatcher(None, None, True)

        # Index in field_name of the currently processed character. Wrap it in
        # a list so that local functions can modify it.
        i = [0]

        error = ValueError('Invalid GNAT encoding for discriminant matcher: {}'
                           .format(repr(field_name)))
        values = []
        ranges = []

        # Make sure we have at least something to parse
        if not field_name:
            raise error

        def consume_char():
            if i[0] >= len(field_name):
                return None
            i[0] += 1
            return field_name[i[0] - 1]

        def go_back():
            assert i[0] > 0
            i[0] -= 1

        def read_number():
            result = ''
            while True:
                c = consume_char()
                if c is None:
                    if not result:
                        raise error
                    return int(result)
                elif '0' <= c <= '9':
                    result += c
                else:
                    if not result:
                        raise error
                    go_back()
                    break
            return int(result)

        while True:
            c = consume_char()
            if c is None:
                break

            elif c == 'R':
                # R<first>T<last> construct: this is a range of matched values
                first = read_number()
                c = consume_char()
                if c != 'T':
                    raise error
                last = read_number()
                ranges.append((first, last))

            elif c == 'S':
                # S<value> construct: this is a single matched value
                values.append(read_number())

            else:
                # Anything else is a parsing error
                raise error

        return DiscriminantMatcher(values, ranges, False)

    def match(self, discr_value):
        """
        Return whether this matches accepts the given discriminant value.

        :param int discr_value: Discriminant value to check.
        :rtype: bool
        """
        if self.match_all:
            return True

        return any(
            discr_value == value for value in self.values
        ) or any(
            first <= discr_value <= last for first, last in self.ranges
        )


def decoded_record(value):
    """
    Decode a record value whose fields involve GNAT encodings.

    GNAT encodings represent variant parts in discriminated records with trees
    of unions and records with names suffixes such as "___XVN" and "___XVE".
    This function decodes a record value so that such encodings are interpreted
    and lifted. This returns an ordered dictionnary that contains all decoded
    fields.

    For instance, the following Ada record type:

       type Discr_Record (N : Natural) is record
          Index : Positive;
          case N is
             when 0      => null;
             when 1      => B : Boolean;
             when 2 .. 9 => C : Character;
             when others => I : Integer;
          end case;
       end record;

    Can be materialized into the corresponding tree of types in GDB (output
    from gnatdbg.debug.print_type_tree):

        %0 (foo__discr_record___XVE : TYPE_CODE_STRUCT):
          n: %1 (natural___XDLU_0__2147483647 : TYPE_CODE_RANGE)
          index: %2 (positive___XDLU_1__2147483647 : TYPE_CODE_RANGE)
          S: %3 (foo__discr_record___n___XVN___S0 : TYPE_CODE_STRUCT):

    Or the more general:

        %0 (foo__discr_record : TYPE_CODE_STRUCT):
          n: %1 (natural___XDLU_0__2147483647 : TYPE_CODE_RANGE)
          index: %2 (positive___XDLU_1__2147483647 : TYPE_CODE_RANGE)
          n___XVN: %3 (foo__discr_record___n___XVN : TYPE_CODE_UNION):
            S0: %4 (foo__discr_record___n___XVN___S0 : TYPE_CODE_STRUCT):
            S1: %5 (foo__discr_record___n___XVN___S1 : TYPE_CODE_STRUCT):
              b: %6 (boolean : TYPE_CODE_BOOL)
            R2T9: %7 (foo__discr_record___n___XVN___R2T9 : TYPE_CODE_STRUCT):
              c: %8 (character : TYPE_CODE_CHAR)
            O: %9 (foo__discr_record___n___XVN___O : TYPE_CODE_STRUCT):
              i: %10 (integer : TYPE_CODE_INT) (4 bytes)

    Values that have either of the forms above will be turned into OrderedDict
    instances such as:

        {'n': 0, 'index': <value>}
        {'n': 1, 'index': <value>, 'b': <value>}
        {'n': 2, 'index': <value>, 'c': <value>}
        {'n': 11, 'index': <value>, 'i': <value>}
    """

    result = OrderedDict()

    union_suffix = '___XVN'

    def process_record(r):
        # Go through all fields in "r", looking for either (un)decoded variant
        # parts (case <discr> is ... end case, in Ada records) or regular
        # fields.
        for f in r.type.fields():
            if f.name == '_parent':
                process_record(r[f.name])

            elif f.name.endswith(union_suffix):
                # This is an undecoded variant part, materialized as an union
                # field whose name has follows the <discr>___XVN pattern.
                # Compute the corresponding discriminant and decode the union.
                discr_name = f.name[:-len(union_suffix)]
                discr = int(value[discr_name])
                process_union(discr, r[f.name])

            elif f.name == 'S':
                # This is a decoded variant part, materialized as a record
                # field that directly contains the fields we are looking for.
                # Just recurse on that record.
                process_record(r[f.name])

            elif not f.name.startswith('_'):
                # This is a regular field (omit compiler-generated ones)
                result[f.name] = r[f.name]

    def process_union(discr_value, u):
        # This union (u) materializes a variant part controlled by the given
        # discriminant value (discr). Look for the variant that matches the
        # given "discr" value.

        # GNAT seems to always generate an "others" matcher in unions for
        # variant parts, so there is always one field that matches the given
        # discriminant. This convoluted iteration replaces a traditional FOR
        # loop to avoid dead code (eases code coverage report analysis).
        field_names = iter([f.name for f in u.type.fields()])
        while True:
            try:
                f = next(field_names)
            except StopIteration: # no-code-coverage
                break

            matcher = DiscriminantMatcher.decode(f)
            if matcher.match(discr_value):
                process_record(u[f])
                break

    process_record(value)
    return result
