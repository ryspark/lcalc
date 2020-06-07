"""Error handling for lc language. Only GenericExceptions should be encountered during running: if another type of error
is raised and makes it all the way to ErrorHandler, it is assumed to be an internal issue.
"""

import sys
from traceback import print_tb

from termcolor import colored


class GenericException(Exception):
    """Templates an error message so that it can be used to throw a lc error instead of a Python error."""

    def __init__(self, msg, exprs=None, start=0, end=-1, diagnosis=True, internal=False):
        if exprs is None:
            exprs = ""
        if isinstance(exprs, str):
            exprs = [exprs]

        self.msg = msg.format(*(colored(expr, attrs=["bold"]) for expr in exprs))  # color expr snippets
        self.expr = exprs[0]  # exprs[0] should be the offending expr that caused the error
        self.start = start
        self.end = end if end != -1 else len(self.expr)  # needed for error display
        self.diagnosis = diagnosis
        self.internal = internal


class ErrorHandler:
    """Context manager that will silently suppress Python errors and raise custom lc errors."""

    def __init__(self, fatal=True):
        self.fatal = fatal
        self.traceback = {}

    def register_file(self, path):
        """Registers path in traceback."""
        self.traceback[path] = (None, None)

    def register_line(self, path, line, line_num):
        """Registers line in traceback given path. Should be called prior to Session run."""
        self.traceback[path] = (line, line_num)

    def remove_line(self, path):
        """Removes line from traceback given path. Should be called after successful Session run."""
        self.traceback[path] = (None, None)

    def throw_error(self, error):
        """Throws error using templated and self.traceback. templated must be a string formatted by template, and
        self.traceback must be a dictionary of file: line_num representing origination of error.
        """
        error_msg = "Traceback:\n"
        for file, (line, line_num) in self.traceback.items():
            if line:
                error_msg += f"  File '{file}', line {line_num}:\n"
                error_msg += f"    {line}\n"

        if error.internal:
            error_msg += colored("[internal] ", "red", attrs=["bold"])

        error_msg += colored("error: ", "red", attrs=["bold"]) + error.msg
        print(error_msg)

        if not error.internal and error.expr and error.diagnosis:
            diagnosis = "  " + error.expr[:error.start]

            diagnosis += colored(error.expr[error.start:error.end], "red", attrs=["bold"])
            diagnosis += error.expr[error.end:] + "\n"

            diagnosis += "  " + " " * error.start
            diagnosis += colored("^" + "~" * (error.end - error.start - 1), "red", attrs=["bold"])

            print(diagnosis)

        if self.fatal:
            sys.exit(1)

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        exit = False
        if exc_type is KeyboardInterrupt:
            self.throw_error(GenericException("keyboard interrupt"))
        elif exc_type is GenericException:
            self.throw_error(exc_val)
        elif exc_type is not None:
            print_tb(exc_tb)
            self.throw_error(GenericException(f"unknown error: '{exc_type.__name__}: {exc_val}'", internal=True))
            exit = True

        return not exit
