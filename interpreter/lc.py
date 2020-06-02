"""Uses implementation of pure lambda calculus/lc language to interpreter .lc files, or run in command-line mode.
Called from lc excutable script.

"""

from interpreter.lang.session import Session


if __name__ == "__main__":
    sess = Session("../common")

    sess.add('#import "common"')

    sess.add("+ 1 1")
    sess.add("* 1 3")
    sess.add("+ 3 2")
    sess.add("** 2 2")

    sess.add("IF (= 5 4) 1 0")

    sess.run()
