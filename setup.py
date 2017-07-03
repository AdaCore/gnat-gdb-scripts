#! /usr/bin/env python

from distutils.core import setup


setup(
    name='GNATdbg',
    version='0.1-dev',
    author='AdaCore',
    author_email='report@adacore.com',
    url='https://github.com/AdaCore/gnat-gdb-scripts',
    description='Python helpers in GDB to deal with the GNAT runtime for Ada',
    packages=['gnatdbg'],

    # This requires to be run in GDB's embedded Python interpreter
    requires=[],
)
