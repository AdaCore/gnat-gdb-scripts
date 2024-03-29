from __future__ import annotations

import atexit
import os
import re
from typing import Optional

import pexpect

from support.expect import EXPECT_DIED, EXPECT_TIMEOUT, ExpectProcess
from support.quotemeta import convert_expression
from support.utils import indent


class GDBSession:
    """
    Handle for a GDB session, to run GDB commands and inspect their output.
    """

    PROMPT_RE = r"\(gdb\) "
    TIMEOUT = 30  # In seconds

    def __init__(self,
                 program: Optional[str] = None,
                 log_file: Optional[str] = None,
                 load_gnatdbg: bool = True):

        # Make sure that the GDB subprogram is terminated with its logs written
        # somewhere before the end of the script.
        atexit.register(self.stop)

        self.log_file = log_file or "gdb.log"
        self.coverage_enabled = False

        # TODO: handle non-native platforms

        # Disable the load of .gdbinit to avoid user configuration
        # interference.
        argv = ["gdb", "--nh"]

        os.environ["TERM"] = "dumb"

        self.proc = ExpectProcess(argv, save_input=True, save_output=True)
        self.alive = True
        _ = self._read_to_next_prompt()

        # If code coverage is enabled, start it before loading gnatdbg
        datafile = os.environ.get("COVERAGE_DATAFILE")
        self.coverage_enabled = datafile is not None
        if self.coverage_enabled:
            rcfile = os.environ["COVERAGE_RCFILE"]
            self.import_coverage()
            self.execute("""python
_cov = coverage.Coverage(data_file={data_file!r},
                         config_file={config_file!r},
                         auto_data=True)
_cov.start()
end""".format(
                data_file=datafile,
                config_file=rcfile
            ))

        # Enable Python backtraces to ease investigation
        self.execute("set python print-stack full")

        # Disable interactive mode, which is bound to create trouble in a
        # testsuite.
        self.execute("set interactive-mode off")

        # Make the output deterministic, independent of the actual terminal
        # size.
        self.execute("set height 0")
        self.execute("set width 80")

        if load_gnatdbg:
            # Automatically load the pretty-printers. Make sure loading happens
            # without error.
            self.test("python import gnatdbg; gnatdbg.setup()", "")

        if program:
            # Only then, load the inferior. Loading gnatdbg before checks that
            # importing it does not rely on the presence of debug information.
            self.test("file {}".format(program),
                      r"Reading symbols from {}...@/done|/"
                      .format(program))

    def import_coverage(self) -> None:
        """
        Import the "coverage" module in GDB's Python session.

        This just does what's necessary to import the "coverage" module: this
        excludes starting the tracing process.
        """
        # TODO: what follows is a hack. We assume here that the "coverage"
        # module reachable in this testsuite script can be imported as-is from
        # the Python intepreter embedded in GDB.
        #
        # This is not generally true, so it can fail. It's not clear at this
        # point how things should be done properly, but we have this in the
        # meantime to at least have one way to compute code coverage in some
        # development setup.

        import coverage
        coverage_path = os.path.dirname(os.path.dirname(coverage.__file__))

        self.execute("python import sys; sys.path.append({})"
                     .format(repr(coverage_path)))
        self.execute("python import coverage")

    def _read_to_next_prompt(self) -> str:
        """
        Read GDB's output until we reach the next prompt.

        Return the output in between. Raise a RuntimeError if GDB dies or if
        timeout is reached.

        :rtype: str
        """
        assert self.alive
        status = self.proc.expect([self.PROMPT_RE], self.TIMEOUT)
        if status is EXPECT_DIED:
            raise RuntimeError("GDB died")
        elif status is EXPECT_TIMEOUT:
            raise RuntimeError("Timeout reached while waiting for GDB")

        assert status == 0
        out, prompt = self.proc.out()
        return out

    def execute(self, command: str) -> None:
        """
        Shortcut for `test` without an expected output.
        """
        return self.test(command, None)

    def test(self, command: str, expected_output: Optional[str]) -> None:
        """
        Send the given command to GDB and check its output.

        :param command: GDB command to send.
        :param expected_output: If None, don't check the command output.
            Otherwise, it must be a quotemeta expression that must match the
            output.
        """
        assert self.alive
        assert self.proc.send(command)
        output = self._read_to_next_prompt().strip().replace("\r", "")
        matcher = (
            convert_expression(expected_output) if expected_output else ""
        )
        if (
            expected_output is not None
            and not re.match(matcher, output)
        ):
            print("")
            print("FAIL: {}".format(command))
            print("Output:")
            print(indent(output))
            print("Does not match the expected:")
            print(indent(expected_output))

    def print_expr(self, expr: str, expected_output: str) -> None:
        """
        Execute the "print" GDB command and check its output.

        :param expr: Expression to print
        :param expected_output: Regular expression that the output must
            match. Note that it must not include the '$NUMBER = ' prefix.
        """
        self.test("print {}".format(expr), "$@NUMBER = " + expected_output)

    @staticmethod
    def find_loc(filename: str, slug: str) -> str:
        """
        Look for a source file location.

        If the location is found, return a string location suitable for GDB's
        break command. Raise a RuntimeError otherwise.

        :param filename: Target source file name.
        :param slug: Source file excerpt for the location. For instance, a
            specific string that is in a comment.
        :return: String location suitable for GDB's break command.
        """
        with open(filename, "r") as f:
            for i, line in enumerate(f, 1):
                if slug in line:
                    return "{}:{}".format(filename, i)
        raise RuntimeError(
            "Could not find location in {} for {}".format(filename, slug)
        )

    def run_to(self, location: str) -> None:
        """
        Start inferior execution until it reaches the given location.

        :param location: String location suitable for GDB's break command.
        """
        self.execute("tbreak {}".format(location))
        self.execute("run")

    def kill(self) -> None:
        """
        Kill the inferior process currently running.
        """
        self.test("kill",
                  "Kill the program being debugged? (y or n)"
                  " [answered Y; input not from terminal]\n"
                  "[Inferior 1 (process @/\d+/) killed]")

    def stop(self) -> None:
        """
        Stop GDB.

        This writes session logs to make post-mortem debugging.
        """
        if not self.alive:
            return

        if self.coverage_enabled:
            self.execute("python _cov.stop(); _cov.save()")
        self.alive = False

        # No matter what, write the session logs to make post-mortem debugging
        # possible.
        with open(self.log_file, "w") as f:
            f.write(self.proc.get_session_logs())
