from gnatdbg.records import DiscriminantMatcher


def evaluate(field_name):
    try:
        print(DiscriminantMatcher.decode(field_name))
    except ValueError as exc:
        print('ValueError({})'.format(exc))
