"""Handles errors in lc interpreter.

"""

import sys


def error():
    """Throws error. All errors in lc are syntax errors."""
    sys.exit(1)
