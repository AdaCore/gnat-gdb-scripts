from __future__ import annotations

import sys
from typing import List

from e3.os.process import Run


def gnatmake(main: str, debug: bool = True, cargs: List[str] = []) -> None:
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
