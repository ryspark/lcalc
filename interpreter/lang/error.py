"""Error handling for lc language. Only SyntaxErrors should be encountered during running: if another type of error is
raised and makes it all the way to ErrorHandler, it is assumed to be an internal issue.
"""

import sys
from traceback import print_tb

from termcolor import colored


def template(msg, exprs=None, internal=False, start=0, end=-1, diagnosis=True):
    """Templates an error message so that it can be used to throw a lc error instead of a Python error. The first
    element of exprs should be the offending expr that caused the error.
    """
    if exprs is None:
        exprs = ""
    elif isinstance(exprs, str):
        exprs = [exprs]

    colored_exprs = (colored(expr, attrs=["bold"]) for expr in exprs)  # color expr snippets
    msg = msg.format(*colored_exprs)

    return f"{'i' if internal else ''}|{msg}|{exprs[0] if exprs else ''}|{start}|{end}|{'d' if diagnosis else ''}"


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

    def throw_error(self, templated):
        """Throws error using templated and self.traceback. templated must be a string formatted by template, and
        self.traceback must be a dictionary of file: line_num representing origination of error.
        """
        internal, msg, expr, start, end, diagnosis = templated.split("|")

        error_msg = "Traceback:\n"
        for file, (line, line_num) in self.traceback.items():
            if line:
                error_msg += f"  File '{file}', line {line_num}:\n"
                error_msg += f"    {line}\n"

        if internal:
            error_msg += colored("[internal] ", "red", attrs=["bold"])

        error_msg += colored("error: ", "red", attrs=["bold"]) + msg
        print(error_msg)

        if not internal and expr and diagnosis:
            if end == "-1":
                end = len(expr)  # needed for counting '~'

            start, end = int(start), int(end)
            diagnosis = "  " + expr[:start] + colored(expr[start:end], "red", attrs=["bold"]) + expr[end:] + "\n"
            diagnosis += "  " + " " * start + colored("^" + "~" * (end - start - 1), "red", attrs=["bold"])

            print(diagnosis)

        if self.fatal:
            sys.exit(1)

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        exit = False
        if exc_type is KeyboardInterrupt:
            self.throw_error(template("keyboard interrupt"))
        elif exc_type in (SyntaxError, OSError):
            self.throw_error(str(exc_val))
        elif exc_type is not None:
            print_tb(exc_tb)
            self.throw_error(template(f"unknown error: '{exc_type.__name__}: {exc_val}'", internal=True))
            exit = True

        return not exit
