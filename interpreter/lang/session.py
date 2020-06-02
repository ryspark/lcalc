"""Session control for lc language. Implementation of lexical parsing to run the lc interpreter, either in command line
mode or file interpretation mode.
"""

import os

from interpreter.lang.lexical import DefineStmt, ExecStmt, ImportStmt, Grammar, NamedFunc


class Session:
    """Governs a lc session, with control over scope of named funcs."""

    def __init__(self):
        self.defines = []    # list of define directives
        self.scope = []      # list of NamedFuncs that exist in the current session
        self.to_exec = []    # list of ExecStmts to execute

        self.flattened = []  # flattened list of all ExecStmt nodes in the current session
        self.results = []    # results after running ExecStmts

    @classmethod
    def from_file(cls, path):
        """Called to load imports, named funcs, and exec stmts into scope from a file. Used to begin process of
        interpreting and running a .lc file.
        """
        assert os.path.exists(path), f"'{path}' not found"
        raise NotImplementedError()

    def add(self, expr):
        """Adds GrammarLC object to the current session. Beta-reduction is lazy and is delayed until run is called."""
        stmt = Grammar.infer(expr, self.defines)

        if isinstance(stmt, ImportStmt):
            self.scope = Session.from_file(stmt.path).scope + self.scope  # append to beginning to avoid name conflicts
        elif isinstance(stmt, DefineStmt):
            self.defines.append(stmt)
        elif isinstance(stmt, NamedFunc):
            self.scope.append(stmt)

        elif isinstance(stmt, ExecStmt):
            self.to_exec.append(stmt)
            self.flattened.extend(stmt.term.tree.flattened.keys())

    def run(self):
        """Runs this session's executable statements by expanding them and then beta-reducing them. Will raise any
        errors that are encountered.
        """
        self._expand_scope()       # expand scope NamedFuncs

        for stmt in self.scope:    # reduce/sub iff stmt is used in ExecStmts
            if stmt.name in self.flattened:
                stmt.sub_all(self.to_exec)

        print("TO_EXEC:\n" + "".join(["    " + str(func) + "\n" for func in self.to_exec]))

        for stmt in self.to_exec:  # run ExecStmts
            self.results.append(stmt.execute())

        print("RESULTS:\n" + "".join(["    " + repr(func) + "\n" for func in sess.results]))

    def _expand_scope(self):
        """Expands any NamedFuncs within self.scope."""
        funcs, seen = {func.name: func for func in self.scope}, []
        for func in self.scope:
            for node, paths in func.term.flattened.items():
                if node in funcs and node not in seen:
                    raise ValueError(f"'{node}' used prior to definition")
                elif node in funcs:
                    for path in paths:
                        funcs[node].sub(func, path)

            seen.append(func.name)

        print("SCOPE:\n" + "".join(["    " + str(func) + "\n" for func in self.scope]))


if __name__ == "__main__":
    sess = Session()

    sess.add("#define lambda <lambda>")

    sess.add("SUCC  := lambda n.lambda f.lambda x.f (n f x)")
    sess.add("+     := lambda m.lambda n.n SUCC m")
    sess.add("**    := lambda m.lambda n.n m")
    sess.add("*     := lambda m.lambda n.lambda f.m (n f)")

    sess.add("+ 1 1")
    sess.add("* 1 3")
    sess.add("+ 3 2")
    sess.add("** 2 2")

    sess.run()
