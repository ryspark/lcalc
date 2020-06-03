"""Handles interactive/command-line mode for lc interpreter. Uses cmd as backend."""

import cmd

from lang.error import throw_error
from lang.session import Session


class Shell(cmd.Cmd):
    """Lambda calculus interpreter shell."""
    intro = "Lambda calculus interpreter :: Python backend\nType '?' or 'help' for more information."
    prompt = "> "
    secondary_prompt = ". "  # used for line continutations
    _tmp_prompt = "> "       # also used for prompt swapping in line continuations

    def __init__(self, sess, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.sess = sess
        self._tmp_line = ""

    def default(self, line):
        """Executes arbitrary lc command."""
        line, add_to_prev = Session.preprocess_line(self._tmp_line + line, self._tmp_line)

        if line and add_to_prev:
            self._tmp_line = line
            self.prompt = self.secondary_prompt

        elif line:
            self._tmp_line = ""
            self.prompt = self._tmp_prompt

            self.sess.add(line)
            self.sess.run()

            if self.sess.results:
                print(self.sess.pop())

    def do_help(self, arg):
        """Doesnt return docs, but rather a short intro."""
        print("Welcome to the lc interpreter!\n\n"
              "Lambda calculus is a Turing-complete language created by Alonzo Church. This \n"
              "interpreter supports pure lambda calculus as imagined by Church as well as \n"
              "named functions, #import statements, and #define statements.\n\n"
              "Try it out by typing 'I := λx.x'.This will bind the lambda term 'λx.x' to a \n"
              "name 'I'. Next, try typing 'I y'. This will apply 'I' to 'y', giving 'y' as \n"
              "the result.")

    def emptyline(self):
        """Do not repeat previous command on empty line."""
        return ""

    def do_EOF(self, arg):
        """Exits interpreter."""
        print()
        return self.do_exit(arg)

    def do_exit(self, arg):
        """Exits interpreter."""
        if arg:
            throw_error(f"Unrecognized token: '{arg}'", fatal=False)
            return False
        return True
