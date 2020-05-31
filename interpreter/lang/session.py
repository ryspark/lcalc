import os

from interpreter.lang.lexical import ExecStmt, ImportStmt, GrammarLC, NamedFunc


class Session:

    def __init__(self, path=None, mode="file"):
        assert mode in ("file", "cmd-line"), f"mode must be 'file' or 'cmd-line', got {mode}"
        assert mode == "cmd-line" or os.path.exists(path), f"'{path}' not found"

        self.path = path
        self.mode = mode

        self.scope = []      # dict of NamedFuncs that exist in the current session
        self.to_exec = []    # list of ExecStmts to execute

        self.flattened = []  # flattened list of all ExecStmt nodes in the current session
        self.results = []    # results after running ExecStmts

    @classmethod
    def from_file(cls, path):
        raise NotImplementedError()

    def add(self, expr):
        stmt = GrammarLC.infer(expr)

        if isinstance(stmt, ImportStmt):
            self.scope.extend(Session.from_file(stmt.run()).scope)

        elif isinstance(stmt, NamedFunc):
            self.scope.append(stmt)

        elif isinstance(stmt, ExecStmt):
            self.to_exec.append(stmt)
            self.flattened.extend(list(stmt.term.tree.flattened.keys()))

    def run(self):
        self._expand_scope()       # expand scope NamedFuncs

        for stmt in self.scope:    # reduce/sub iff stmt is used in ExecStmts
            if stmt.name in self.flattened:
                stmt.run(self.to_exec)

        print("TO_EXEC:\n" + "".join(["    " + str(func) + "\n" for func in self.to_exec]))

        for stmt in self.to_exec:  # run ExecStmts
            self.results.append(stmt.run())

    def _expand_scope(self):
        """Expands any NamedFuncs within self.scope."""
        funcs, seen = {func.name: func for func in self.scope}, []
        for func in self.scope:
            for node, paths in func.flattened.items():
                if node in funcs and node not in seen:
                    raise ValueError(f"'{node}' used prior to definition")
                elif node in funcs:
                    for path in paths:
                        funcs[node].sub(func, path)

            seen.append(func.name)

        print("SCOPE:\n" + "".join(["    " + str(func) + "\n" for func in self.scope]))


if __name__ == "__main__":
    from interpreter.lang.numerical import church_numeral

    sess = Session(mode="cmd-line")

    sess.add("SUCC  := λn.λf.λx.f (n f x)")
    sess.add("+     := λm.λn.n SUCC m")
    sess.add("**    := λm.λn.n m")
    sess.add(f"** {church_numeral(2)} {church_numeral(1)}")

    print("FLATTENED:\n" + "".join(["    " + str(func) + "\n" for func in sess.flattened]))
    print()

    sess.run()
    print("RESULTS:\n" + "".join(["    " + repr(func) + "\n" for func in sess.results]))
