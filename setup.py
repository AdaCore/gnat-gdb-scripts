#! /usr/bin/env python

import os.path

from setuptools import setup


# The actual version is stored in a separate file to ease automatic updates
version_file = os.path.join(os.path.dirname(__file__), "VERSION")
with open(version_file) as f:
    version = f.read().strip()


setup(
    name="GNATdbg",
    version=version,
    author="AdaCore",
    author_email="report@adacore.com",
    url="https://github.com/AdaCore/gnat-gdb-scripts",
    description="Python helpers in GDB to deal with the GNAT runtime for Ada",
    packages=["gnatdbg"],
    # This requires to be run in GDB's embedded Python interpreter
    requires=[],
    package_data={"gnatdbg": ["py.typed"]},
)
