import glob
import os.path

from gnatpython.testsuite import Testsuite as BaseTestsuite

from support.gdb import GDBSession
from support.python_driver import PythonDriver


class Testsuite(BaseTestsuite):
    TEST_SUBDIR = 'tests'
    DRIVERS = {
        'python': PythonDriver,
    }

    def add_options(self):
        self.main.add_option(
            '--coverage', '-C', action='store_true',
            help='Compute gnatdbg code coverage'
        )

    @property
    def coverage_enabled(self):
        return self.global_env['options'].coverage

    @property
    def coverage_dir(self):
        return os.path.join(self.global_env['output_dir'], 'coverage')

    @property
    def coverage_rcfile(self):
        return os.path.join(
            os.path.dirname(os.path.abspath(__file__)),
            '..',
            'coverage.ini'
        )

    def tear_up(self):
        super(Testsuite, self).tear_up()

        if self.coverage_enabled:
            # Create a directory that we'll use to:
            #
            #   1) collect coverage data for each testcase;
            #   2) generate the HTML report.
            os.mkdir(self.coverage_dir)

            self.global_env['coverage_dir'] = self.coverage_dir
            os.environ['COVERAGE_DIR'] = self.coverage_dir
            os.environ['COVERAGE_RCFILE'] = self.coverage_rcfile

    def tear_down(self):
        if self.coverage_enabled:
            # Process coverage data with the same Python interpreter and
            # "coverage" package that was used to produce them. To achieve
            # this, spawn GDB just like testcases.
            gdb = GDBSession(
                'doesnotexist',
                log_file=os.path.join(self.coverage_dir, 'gdb.log'),
                load_gnatdbg=False
            )

            gdb.execute('python import glob, coverage')

            # Consolidate coverage data for each testcase and generate both a
            # sumary textual report on the standard output and a detailed HTML
            # report.
            gdb.execute('''python
c = coverage.Coverage(data_file={data_file!r}, config_file={config_file!r})
c.combine(glob.glob({data_files_glob!r}))
c.html_report(directory={coverage_dir!r}, title='gnatdbg coverage report')
end'''.format(
                data_file=os.path.join(self.coverage_dir, '.coverage'),
                data_files_glob=os.path.join(self.coverage_dir, '*.coverage'),
                config_file=self.coverage_rcfile,
                coverage_dir=self.coverage_dir
            ))

            html_index = os.path.join(self.coverage_dir, 'index.html')
            assert os.path.exists(html_index)
            print('Detailed HTML coverage report available at:'
                  ' {}'.format(html_index))

        super(Testsuite, self).tear_down()
