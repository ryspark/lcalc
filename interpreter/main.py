"""Uses implementation of pure lambda calculus/lc language to interpreter .lc files/run in command-line mode. Also
provides error handling. Called from lc excutable script.

Error handling is implemented here instead of Session to make sure that all possible Python exceptions are converted
to lambda calculus errors.
"""

import argparse
import os

from lang.error import ErrorHandler
from lang.shell import Shell
from lang.session import Session


def main():
    """Runs lc interpreter. Called from lc executable script."""
    with ErrorHandler() as error_handler:
        parser = argparse.ArgumentParser()
        parser.add_argument("file", help="file to interpret and run (if empty, goes to command-line mode)", nargs="?")
        args = parser.parse_args()

        COMMON = os.path.join(__file__[:__file__.rfind("/")], "../common")

        if args.file is not None:
            sess = Session(error_handler, args.file, COMMON, cmd_line=False)
            sess.run()

            for node in sess.results:
                print(node.tree.expr)

        else:
            Shell(Session(error_handler, "<in>", COMMON, cmd_line=True)).cmdloop()
