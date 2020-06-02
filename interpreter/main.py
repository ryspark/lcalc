"""Uses implementation of pure lambda calculus/lc language to interpreter .lc files, or run in command-line mode.
Called from lc excutable script.

"""

import argparse
import os

from lang.session import Session


def main():
    """Runs lc interpreter. Called from lc executable script."""
    parser = argparse.ArgumentParser()
    parser.add_argument("file", help="file to interpret and run (if empty, goes to command-line mode)", nargs="?")
    args = parser.parse_args()

    if args.file is not None:
        sess = Session.from_file(args.file, os.path.join(__file__[:__file__.rfind("/")], "../common"))
        sess.run()

        for node in sess.results:
            print(node.tree.expr)

    else:
        raise NotImplementedError("command-line mode not yet implemented")
