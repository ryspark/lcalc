"""Uses implementation of pure lambda calculus/lc language to interpreter .lc files/run in command-line mode. Also uses
error handling context manager. Called from lc excutable script.

Python version must be >=3.6, because error handling requires that dicts are insertion-ordered.
"""

import argparse
import os
import sys

from lang.error import ErrorHandler
from lang.shell import Shell
from lang.session import Session


def main():
    """Runs lc interpreter. Called from lc executable script."""
    assert sys.version_info >= (3, 6), "lc cannot be run with python < 3.6"

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
            Shell(Session(error_handler, Session.SH_FILE, COMMON, cmd_line=True)).cmdloop()
