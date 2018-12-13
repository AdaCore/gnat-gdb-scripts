from support.build import gnatmake
from support.gdb import GDBSession


gdb = GDBSession()


def dm(label):
    return 'DiscriminantMatcher({})'.format(label)


def error(field_name):
    return ("ValueError(Invalid GNAT encoding for discriminant matcher: {})"
            .format(repr(field_name)))


tests = [
    # Regular cases
    ('O',       dm('others')),
    ('S1',      dm('1')),
    ('S1R2T3',  dm('1 | 2 .. 3')),
    ('S1S2S3',  dm('1 | 2 | 3')),
    ('R1T100',  dm('1 .. 100')),

    # Erroneous cases
    ('',        error),
    ('a',       error),
    ('S',       error),
    ('Sa',      error),
    ('R',       error),
    ('Ra',      error),
    ('R1a',     error),
    ('R1T',     error),
    ('R1Ta',    error),
    ('R1T10a',  error),
]

gdb.test('source helpers.py', '')

for field_name, baseline in tests:
    if baseline == error:
        baseline = error(field_name)
    gdb.test('python evaluate({})'.format(repr(field_name)),
             baseline)
