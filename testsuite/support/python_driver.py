import os
import os.path
import sys

from support.base_driver import BaseDriver


class PythonDriver(BaseDriver):

    TIMEOUT = 300
    PY_FILE = 'test.py'

    def run(self):
        # Make some Python packages from this repository available to the
        # testcase:
        py_path = [
            # The testsuite's "support" package
            os.path.join(self.testsuite_dir),

            # gnatdbg itself
            os.path.join(self.testsuite_dir, '..')
        ] + self.env_path_split(os.environ.get('PYTHONPATH'))

        if self.run_and_check(
            [sys.executable, self.PY_FILE],
            {'PYTHONPATH': self.env_path_format(py_path)},
            error_on_output=True,
        ):
            self.result.set_status('PASSED')
