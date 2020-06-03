"""Uses implementation of pure lambda calculus/lc language to interpreter .lc files, or run in command-line mode.
Called from lc excutable script.

"""

import argparse
import os

from lang.interactive import Shell
from lang.session import Session


def main():
    """Runs lc interpreter. Called from lc executable script."""
    parser = argparse.ArgumentParser()
    parser.add_argument("file", help="file to interpret and run (if empty, goes to command-line mode)", nargs="?")
    args = parser.parse_args()

    COMMON = os.path.join(__file__[:__file__.rfind("/")], "../common")

    if args.file is not None:
        sess = Session.from_file(args.file, COMMON)
        sess.run()

        for node in sess.results:
            print(node.tree.expr)

    else:
        Shell(Session(common_path=COMMON, cmd_line=True)).cmdloop()
