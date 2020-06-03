"""Handles interactive/command-line mode for lc interpreter. Uses cmd as backend."""

import cmd

from lang.error import throw_error


class Shell(cmd.Cmd):
    intro = "Lambda calculus interpreter :: Python backend\nType '?' or 'help' for more information."
    prompt = "> "

    def __init__(self, sess, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.sess = sess

    def default(self, line):
        try:
            self.sess.add(line)
            self.sess.run()
        except ValueError:
            throw_error(f"Unrecognized token: '{line}'", fatal=False)

        if self.sess.results:
            print(self.sess.pop_last())

    def do_help(self, arg):
        print("Welcome to the lc interpreter!\n\n"
              "Lambda calculus is a Turing-complete language created by Alonzo Church. This \n"
              "interpreter supports pure lambda calculus as imagined by Church as well as \n"
              "named functions, #import statements, and #define statements.\n\n"
              "Try it out by typing 'I := λx.x'.This will bind the lambda term 'λx.x' to a \n"
              "name 'I'. Next, try typing 'I y'. This will apply 'I' to 'y', giving 'y' as \n"
              "the result.")

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
