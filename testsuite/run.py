#! /usr/bin/env python

"""
Usage::

    run.py [OPTIONS]

Run the gnatdbg testsuite.
"""

import os

from support import Testsuite


if __name__ == '__main__':
    Testsuite(os.path.dirname(__file__)).testsuite_main()
