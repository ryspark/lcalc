"""Error handling for lcalc language. Only GenericExceptions should be encountered during running: if another type of
error is raised and makes it all the way to ErrorHandler, it is assumed to be an internal issue.
"""

import sys

from termcolor import colored


class GenericException(Exception):
    """Templates an error/warning message so that it can be used to throw a lcalc error/warning. Essentially just a
    wrapper around parse_args.
    """

    def __init__(self, msg, exprs=None, start=0, end=-1, diagnosis=True, internal=False):
        """Parses args for GenericException or warning."""
        if exprs is None:
            exprs = ""
        if isinstance(exprs, str):
            exprs = [exprs]

        self.msg = msg.format(*(colored(expr, attrs=["bold"]) for expr in exprs))  # color expr snippets
        self.expr = exprs[0]  # exprs[0] should be the offending expr that caused the error
        self.end = end if end != -1 else len(self.expr)  # needed for error display

        self.start = start
        self.diagnosis = diagnosis
        self.internal = internal


class ErrorHandler:
    """Context manager that will silently suppress Python errors and raise custom lcalc errors/warnings."""
    ERROR = "red"
    WARNING = "magenta"

    def __init__(self, fatal=True):
        self.fatal = fatal
        self.traceback = {}

    def register_file(self, path):
        """Registers path in traceback."""
        self.traceback[path] = (None, None)

    def register_line(self, path, line, line_num):
        """Registers line in traceback given path. Should be called prior to Session add/run."""
        self.traceback[path] = (line, line_num)

    def remove_line(self, path):
        """Removes line from traceback given path. Should be called after successful Session add/run."""
        self.traceback[path] = (None, None)

    @staticmethod
    def diagnose(error, warning=False):
        """Returns offending part of error.expr highlighted and bolded."""
        color = ErrorHandler.WARNING if warning else ErrorHandler.ERROR

        diagnosis = "  " + error.expr[:error.start]

        end = max(error.end, 1)
        diagnosis += colored(error.expr[error.start:end], color, attrs=["bold"])
        diagnosis += error.expr[end:] + "\n"

        diagnosis += "  " + " " * error.start
        diagnosis += colored("^" + "~" * (end - error.start - 1), color, attrs=["bold"])

        return diagnosis

    def warn(self, *args, **kwargs):
        """Generates and prints runtime warning message based on args."""
        error = GenericException(*args, **kwargs)

        file, (line, line_num) = next(iter(self.traceback.items()))
        col = line.index(error.expr) + error.start  # assumes error.expr in line

        error_msg = colored(f"{file}:{line_num}:{col}: ", attrs=["bold"])
        error_msg += colored("warning: ", ErrorHandler.WARNING, attrs=["bold"]) + error.msg

        print(error_msg)

        if not error.internal and error.expr and error.diagnosis:
            print(ErrorHandler.diagnose(error, warning=True))

    def throw(self, error):
        """Throws error using error and self.traceback. error must be a GenericException, and self.traceback must be a
        dict of file: (line, line_num) representing origination of error.
        """
        error_msg = ""
        lines = 0
        for file, (line, line_num) in self.traceback.items():  # assfumes dict is insertion-ordered
            if line:
                error_msg += f"  File '{file}', line {line_num}:\n"
                error_msg += f"    {line}\n"
                lines += 1

        if lines > 1:
            error_msg = "Traceback:\n" + error_msg

        if error.internal:
            error_msg += colored("[internal] ", ErrorHandler.ERROR, attrs=["bold"])

        error_msg += colored("error: ", ErrorHandler.ERROR, attrs=["bold"]) + error.msg
        print(error_msg)

        if not error.internal and error.expr and error.diagnosis:
            print(ErrorHandler.diagnose(error))

        if self.fatal:
            sys.exit(1)
        self.traceback = {}  # if error occurred, reset traceback (no need if error is fatal)

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        do_exit = False
        if exc_type is KeyboardInterrupt:
            self.throw(GenericException("keyboard interrupt"))
        elif exc_type is SystemExit:
            do_exit = True
        elif exc_type is RecursionError:
            self.throw(GenericException("beta normal form might exist, but maximum recursion depth exceeded"))
        elif exc_type is GenericException:
            self.throw(exc_val)
        elif exc_type is not None:
            self.throw(GenericException(f"unknown error: '{exc_type.__name__}: {exc_val}'", internal=True))
            do_exit = True

        return not do_exit
