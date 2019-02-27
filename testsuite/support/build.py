import sys

from gnatpython.ex import Run


def gnatmake(main, debug=True, cargs=[]):
    """
    Run GNATmake on the given main source file.

    This exits the Python interpreter if there is an error.

    :param str main: Filename for the main source file. For instance:
        "main.adb".
    :param list[str] cargs: Arguments for GNATmake's -cargs section.
    """
    # TODO: handle non-native platforms
    argv = ['gnatmake', main]
    if debug:
        argv.append('-g')

    if cargs:
        argv.extend(['-cargs'] + cargs)

    p = Run(argv)
    if p.status:
        print('gnatmake failed:')
        print('$ ' + ' '.join(argv))
        print(p.out)
        sys.exit(1)
