from gnatdbg.records import decoded_record


def print_record(name, value):
    print('== {} =='.format(name))
    for k, v in decoded_record(value).items():
        print('  {}: {}'.format(k, v))
