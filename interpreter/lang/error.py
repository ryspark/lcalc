"""Error handling for lc language."""

import sys

import termcolor


def throw_error(msg, fatal=True):
    termcolor.cprint(msg)

    if fatal:
        sys.exit(1)
