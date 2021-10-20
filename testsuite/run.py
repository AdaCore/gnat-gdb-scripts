#! /usr/bin/env python

from __future__ import annotations

import argparse
import os
import sys

from e3.testsuite import Testsuite as BaseTestsuite

from support.gdb import GDBSession
from support.python_driver import PythonDriver
from support.utils import TestsuiteConfig


class Testsuite(BaseTestsuite):

    tests_subdir = "tests"
    test_driver_map = {
        "python": PythonDriver,
    }

    def add_options(self, parser: argparse.ArgumentParser) -> None:
        parser.add_argument(
            "--coverage", "-C", action="store_true",
            help="Compute gnatdbg code coverage"
        )
        parser.add_argument(
            "--no-auto-pythonpath", "-A", action="store_true",
            help="Do not update PYTHONPATH to reach gnatdbg"
        )

    def set_up(self) -> None:
        super().set_up()

        assert self.main.args
        ts_config = TestsuiteConfig(
            no_auto_pythonpath=self.main.args.no_auto_pythonpath,
            coverage=self.main.args.coverage,
            coverage_dir=os.path.join(self.output_dir, "coverage"),
            coverage_rcfile=os.path.join(self.root_dir, "coverage.ini"),
        )
        self.env.ts_config = ts_config

        if ts_config.coverage:
            # Create a directory that we"ll use to:
            #
            #   1) collect coverage data for each testcase;
            #   2) generate the HTML report.
            os.mkdir(ts_config.coverage_dir)
            os.environ["COVERAGE_DIR"] = ts_config.coverage_dir
            os.environ["COVERAGE_RCFILE"] = ts_config.coverage_rcfile

    def tear_down(self) -> None:
        ts_config: TestsuiteConfig = self.env.ts_config
        if ts_config.coverage:
            # Process coverage data with the same Python interpreter and
            # "coverage" package that was used to produce them. To achieve
            # this, spawn GDB just like testcases.
            gdb = GDBSession(
                log_file=os.path.join(ts_config.coverage_dir, "gdb.log"),
                load_gnatdbg=False
            )
            gdb.import_coverage()

            # Consolidate coverage data for each testcase and generate both a
            # sumary textual report on the standard output and a detailed HTML
            # report.
            script = os.path.join(self.working_dir, "coverage_script.py")
            with open(script, "w") as f:
                f.write("""
import glob

c = coverage.Coverage(data_file={data_file!r}, config_file={config_file!r})
c.combine(glob.glob({data_files_glob!r}))
c.html_report(directory={coverage_dir!r}, title="gnatdbg coverage report")
end""".format(
                    data_file=os.path.join(
                        ts_config.coverage_dir, ".coverage"
                    ),
                    data_files_glob=os.path.join(
                        ts_config.coverage_dir, "*.coverage"
                    ),
                    config_file=ts_config.coverage_rcfile,
                    coverage_dir=ts_config.coverage_dir
                ))
            gdb.execute(f"source {script}")

            html_index = os.path.join(ts_config.coverage_dir, "index.html")
            assert os.path.exists(html_index)
            print("Detailed HTML coverage report available at:"
                  " {}".format(html_index))

        super().tear_down()


if __name__ == "__main__":
    sys.exit(Testsuite().testsuite_main())
