"""Session control for lc language. Implementation of lexical parsing to run the lc interpreter, either in command line
mode or file interpretation mode.
"""

import os

from interpreter.lang.lexical import DefineStmt, ExecStmt, ImportStmt, Grammar, NamedFunc


class Session:
    """Governs a lc session, with control over scope of named funcs."""

    def __init__(self, common_path=""):
        self.common_path = common_path  # path to common lc lib

        self.defines = []    # list of define directives
        self.scope = []      # list of NamedFuncs that exist in the current session
        self.to_exec = []    # list of ExecStmts to execute

        self.flattened = []  # flattened list of all ExecStmt nodes in the current session
        self.results = []    # results after running ExecStmts

    @classmethod
    def from_file(cls, path, common_path):
        """Called to load imports, named funcs, and exec stmts into scope from a file. Used to begin process of
        interpreting and running a .lc file. path must be an absolute path, and common path must be the path to the
        common lc lib directory.
        """
        sess = cls(common_path)

        exprs = []
        add_to_prev = False

        with open(path, "r") as file:
            for line in file:
                line = Grammar.preprocess(line)

                line = cls._remove_comments(line)
                if not line.isspace() and line and not add_to_prev:
                    exprs.append(line)
                elif add_to_prev:
                    prev = exprs.pop(-1)
                    exprs.append((prev + line, prev))

                if line.count("(") > line.count(")"):
                    add_to_prev = True
                else:
                    add_to_prev = False

        for expr in exprs:
            if isinstance(expr, tuple):
                sess.add(*expr)
            else:
                sess.add(expr)

        return sess

    @staticmethod
    def _remove_comments(expr):
        """Removes comments from expr."""
        if "--" in expr:
            expr = expr[:expr.index("--")]  # get rid of comments
        return expr

    def add(self, expr, original_expr=None):
        """Adds Grammar object to the current session. Beta-reduction is lazy and is delayed until run is called."""
        if original_expr is None:
            original_expr = expr

        for stmt in self.defines:
            expr = stmt.replace(expr)

        expr = Session._remove_comments(expr)
        if not expr:
            return None

        stmt = Grammar.infer(expr, original_expr)

        if isinstance(stmt, ImportStmt):
            for path in self._get_paths(stmt.path):
                self.scope = Session.from_file(path, self.common_path).scope + self.scope

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

        print("RESULTS:\n" + "".join(["    " + repr(func) + "\n" for func in self.results]))

    def _get_paths(self, path):
        """Returns [path] if path != 'common' else absolute paths of common lc files."""
        if path == "common":
            return [os.path.abspath(f"{self.common_path}/{path}") for path in os.listdir(self.common_path)]
        return [os.path.abspath(path)]

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
