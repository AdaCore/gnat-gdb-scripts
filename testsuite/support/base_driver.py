import os.path

from gnatpython import fileutils
from gnatpython.ex import PIPE, Run, STDOUT
from gnatpython.testsuite.driver import TestDriver


class BaseDriver(TestDriver):
    """
    Base class to provide common test driver helpers.

    Ideally, these should end up in GNATpython, but this base class acts as a
    staging area: once it has been proven that some feature is useful, it may
    be easier to submit it upstream...
    """

    TIMEOUT = None

    def tear_up(self):
        super(BaseDriver, self).tear_up()
        self.create_test_workspace()

    def set_failure(self, message):
        if self.expect_failure:
            self.result.set_status('XFAIL', '{}{}'.format(
                message,
                ' ({})'.format(self.expect_failure_comment)
                if self.expect_failure_comment else ''
            ))
        else:
            self.result.set_status('FAILED', message)

    def set_passed(self):
        if self.expect_failure:
            msg = (
                'Failure was expected: {}'.format(self.expect_failure_comment)
                if self.expect_failure_comment else None
            )
            self.result.set_status('UOK', msg)
        else:
            self.result.set_status('PASSED')

    # Convenience path builders

    @property
    def testsuite_dir(self):
        """Return the absolute path to the testsuite root directory."""
        result = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                              '..')
        return os.path.abspath(result)

    @property
    def test_dir(self):
        """Return the path of the current testcase directory."""
        return self.test_env['test_dir']

    def working_dir(self, *args):
        """
        Return the working dir, plus any path elements joined to it if passed
        in *args.
        """
        return os.path.join(self.global_env['working_dir'],
                            self.test_env['test_name'], *args)

    def create_test_workspace(self):
        """
        Create a test workspace.

        This function copies the test sources into the working directory.
        """

        fileutils.sync_tree(self.test_dir, self.working_dir())

    def env_path_split(self, path_list):
        """
        Split a *PATH environment variable into a list of paths.

        :param str path_list: Content of a *PATH environment variable, for
            instance PYTHONPATH.
        :rtype: list[str]
        """
        return path_list.split(os.path.pathsep) if path_list else []

    def env_path_format(self, path_list):
        """
        Format a list of paths for a *PATH environment variable.

        :param list[str] path_list: List of paths.
        :rtype: str
        """
        return os.path.pathsep.join(path_list)

    #
    # Run helpers
    #

    def run_and_check(self, argv, env={}, error_on_output=False):
        """
        Run a subprocess with `argv`.

        If it exits with status code 0, return True. Otherwise, set failure
        status for this testcase and return False. In any case, log output to
        "processes.log" if not empty.
        """
        program = argv[0]

        passed_env = dict(os.environ)
        passed_env.update(env)
        p = Run(argv, cwd=self.working_dir(), env=passed_env,
                timeout=self.TIMEOUT,
                output=PIPE,
                error=STDOUT)

        if p.out:
            with open(self.working_dir('processes.log'), 'a') as f:
                f.write('== Output for {} ==\n'.format(' '.join(argv)))
                f.write(p.out)
            if error_on_output:
                msg = 'Output of {} is not empty'.format(program)
                self.result.set_status('FAILED', msg)
                self.result.actual_output += msg + ':\n' + p.out
                return False

        if p.status != 0:
            msg = '{} returned status code {}'.format(program, p.status)
            self.result.set_status('FAILED', msg)
            self.result.actual_output += msg + '\n' + p.out
            return False

        return True
