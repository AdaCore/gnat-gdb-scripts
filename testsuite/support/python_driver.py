from __future__ import annotations

import glob
import logging
import os
import os.path
import sys
from typing import List

from e3.testsuite.driver.classic import ClassicTestDriver

from support.utils import TestsuiteConfig, env_path_split, env_path_format


class PythonDriver(ClassicTestDriver):

    TIMEOUT = 300
    PY_FILE = "test.py"

    def run(self) -> None:
        ts_config: TestsuiteConfig = self.env.ts_config

        # Make some Python packages from this repository available to the
        # testcase:
        py_path = [
            # The testsuite"s "support" package
            os.path.join(self.env.root_dir),
        ]

        # gnatdbg itself
        if not ts_config.no_auto_pythonpath:
            py_path.append(os.path.join(self.env.root_dir, ".."))

        py_path += env_path_split(os.environ.get("PYTHONPATH"))

        env = dict(os.environ)
        env["PYTHONPATH"] = env_path_format(py_path)
        if ts_config.coverage:
            env["COVERAGE_DATAFILE"] = os.path.join(
                ts_config.coverage_dir, self.test_env["test_name"] + ".coverage"
            )

        self.shell([sys.executable, "test.py"], env=env)

        # Forward GDB session logs to users, for post-mortem investigation
        for log_file in sorted(glob.glob(self.working_dir("*.log"))):
            with open(log_file) as f:
                log_content = f.read()

            self.result.log += (
                f"== Content of: {os.path.basename(log_file)} =="
                f"\n\n{log_content}"
                "\n\n== END =="
            )

    def compute_failures(self) -> List[str]:
        return ["Unexpected output"] if self.output.log else []
