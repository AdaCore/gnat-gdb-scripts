from gnatpython.testsuite import Testsuite as BaseTestsuite

from support.python_driver import PythonDriver


class Testsuite(BaseTestsuite):
    TEST_SUBDIR = 'tests'
    DRIVERS = {
        'python': PythonDriver,
    }
