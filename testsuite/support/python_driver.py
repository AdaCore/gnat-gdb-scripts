import glob
import logging
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
        ]

        # gnatdbg itself
        if not self.global_env['options'].no_auto_pythonpath:
            py_path.append(os.path.join(self.testsuite_dir, '..'))

        py_path += self.env_path_split(os.environ.get('PYTHONPATH'))

        env = {'PYTHONPATH': self.env_path_format(py_path)}
        coverage_dir = self.global_env.get('coverage_dir')
        if coverage_dir:
            env['COVERAGE_DATAFILE'] = os.path.join(
                coverage_dir, self.test_env['test_name'] + '.coverage'
            )

        if self.run_and_check([sys.executable, self.PY_FILE],
                              env, error_on_output=True):
            self.result.set_status('PASSED')

        # Forward GDB session logs to users, for post-mortem investigation
        for log_file in sorted(glob.glob(self.working_dir("*.log"))):
            with open(log_file) as f:
                log_content = f.read()

            logging.debug("== Content of: {} ==\n\n{}\n\n== END ==".format(
                os.path.basename(log_file),
                log_content
            ))
