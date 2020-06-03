"""Session control for lc language. Implementation of lexical parsing to run the lc interpreter, either in command line
mode or file interpretation mode.
"""

import os

from lang.lexical import DefineStmt, ExecStmt, ImportStmt, Grammar, NamedFunc


class Session:
    """Governs a lc session, with control over scope of named funcs."""

    def __init__(self, path="<in>", common_path="", cmd_line=True):
        self.path = path                # used for error messages
        self.common_path = common_path  # path to common lc lib
        self.cmd_line = cmd_line        # whether or not in command-line mode

        self.defines = []    # list of define directives
        self.scope = []      # list of NamedFuncs that exist in the current session
        self.to_exec = []    # list of ExecStmts to execute

        self.flattened = []  # flattened list of all ExecStmt nodes in the current session
        self.results = []    # results after running ExecStmts

        self.last_expansion = 0

    @classmethod
    def from_file(cls, path, common_path):
        """Called to load imports, named funcs, and exec stmts into scope from a file. Used to begin process of
        interpreting and running a .lc file. path must be an absolute path, and common path must be the path to the
        common lc lib directory.
        """
        sess = cls(path, common_path, cmd_line=False)

        exprs = []
        add_to_prev = False

        with open(path, "r") as file:
            for line in file:
                __, add_to_prev = cls.preprocess_line(line, add_to_prev, exprs)

        for expr in exprs:
            if isinstance(expr, tuple):
                sess.add(*expr)
            else:
                sess.add(expr)

        return sess

    @staticmethod
    def preprocess_line(line, add_to_prev, exprs=None):
        """Preprocesses a line from a file or command-line. In command-line mode, exprs can be ignored (used to keep
        track of file's exprs), but add_to_prev will indicate whether a line continuation is necessary. Returns
        updated value of line and add_to_prev. Must be called before calling run.
        """
        if "--" in line:
            line = line[:line.index("--")]  # get rid of comments

        line = Grammar.preprocess(line)
        if exprs is not None:
            if not line.isspace() and line and not add_to_prev:
                exprs.append(line)
            elif add_to_prev:
                prev = exprs.pop()
                exprs.append((prev + line, prev))

        return line, line.count("(") > line.count(")")

    def add(self, expr, original_expr=None):
        """Adds Grammar object to the current session. Beta-reduction is lazy and is delayed until run is called."""
        if original_expr is None:
            original_expr = expr

        for stmt in self.defines:
            expr = stmt.replace(expr)
        stmt = Grammar.infer(expr, original_expr)

        if isinstance(stmt, ImportStmt):
            for path in self._get_paths(stmt.path):
                self.scope = Session.from_file(path, self.common_path).scope + self.scope

        elif isinstance(stmt, DefineStmt):
            self.defines.append(stmt)

        elif isinstance(stmt, NamedFunc):
            self.scope.append(stmt)

        elif isinstance(stmt, ExecStmt):
            if self.cmd_line:
                self.to_exec = [stmt]
                self.flattened = list(stmt.term.tree.flattened.keys())
            else:
                self.to_exec.append(stmt)
                self.flattened.extend(stmt.term.tree.flattened.keys())

    def run(self):
        """Runs this session's executable statements by expanding them and then beta-reducing them. Will raise any
        errors that are encountered.
        """
        if self.last_expansion != len(self.scope):
            self._expand_scope()       # expand NamedFuncs in scope

        for stmt in self.scope:    # reduce/sub iff stmt is used in ExecStmts
            if stmt.name in self.flattened:
                stmt.sub_all(self.to_exec)

        for stmt in self.to_exec:  # run ExecStmts
            if self.cmd_line:
                self.results = [stmt.execute()]
                self.to_exec.remove(stmt)
            else:
                self.results.append(stmt.execute())

    def pop(self):
        """Returns and removes last result. Used in command-line mode."""
        return self.results.pop().tree.expr

    def _get_paths(self, path):
        """Returns [path] if path != 'common' else absolute paths of common lc files."""
        if path == "common":
            return [f"{self.common_path}/{file}" for file in os.listdir(self.common_path)]
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

        self.last_expansion = len(self.scope)
