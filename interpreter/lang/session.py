"""Session control for lc language. Implementation of lexical parsing to run the lc interpreter, either in command line
mode or file interpretation mode.
"""

import os

from lang.error import GenericException
from lang.lexical import DefineStmt, ExecStmt, ImportStmt, Grammar, NamedFunc


class Session:
    """Governs a lc session, with control over scope of named funcs."""

    def __init__(self, error_handler, path, common_path, cmd_line):
        self.error_handler = error_handler
        self.error_handler.register_file(path)

        self.path = path                # used for error messages
        self.common_path = common_path  # path to common lc lib
        self.cmd_line = cmd_line        # whether or not in command-line mode

        self.defines = []    # list of define directives
        self.scope = []      # list of NamedFuncs that exist in the current session that have not been expanded
        self.memory = []     # list of NamedFuncs that exist in the current session that have been expanded
        self.to_exec = {}    # dict of line num: ExecStmts to execute

        self.flattened = []  # flattened list of all ExecStmt nodes in the current session
        self.results = []    # results after running ExecStmts

        if self.cmd_line:
            self.error_handler.fatal = False

        if path != "<in>":
            exprs = []
            add_to_prev = False

            try:
                with open(path, "r") as file:
                    for line_num, line in enumerate(file):
                        __, add_to_prev = self.preprocess_line(line, line_num + 1, add_to_prev, exprs)
            except (OSError, IsADirectoryError, FileNotFoundError):
                raise GenericException("'{}' could not be opened", path, diagnosis=False)

            for expr in exprs:
                self.add(*expr)

    @staticmethod
    def preprocess_line(line, line_num, add_to_prev, exprs=None):
        """Preprocesses a line from a file or command-line. In command-line mode, exprs can be ignored (used to keep
        track of file's exprs), but add_to_prev will indicate whether a line continuation is necessary. Returns
        updated value of line and add_to_prev. Must be called before calling run.
        """
        if "--" in line:
            line = line[:line.index("--")]  # get rid of comments

        line = Grammar.preprocess(line)
        if exprs is not None:
            if not line.isspace() and line and not add_to_prev:
                exprs.append((line, line_num))
            elif add_to_prev:
                prev = exprs.pop()
                exprs.append((prev + line, line_num, prev))

        return line, line.count("(") > line.count(")")

    def add(self, expr, line_num):
        """Adds Grammar object to the current session. Beta-reduction is lazy and is delayed until run is called."""
        self.error_handler.register_line(self.path, expr, line_num)  # in case error is raised

        for stmt in self.defines:
            expr = stmt.replace(expr)
        stmt = Grammar.infer(expr)

        if isinstance(stmt, ImportStmt):
            for path in self._get_paths(stmt.path):
                self.scope = Session(self.error_handler, path, self.common_path, self.cmd_line).scope + self.scope
                # on import, ExecStmts from the imported module will not be run

        elif isinstance(stmt, DefineStmt):
            self.defines.append(stmt)

        elif isinstance(stmt, NamedFunc):
            self.scope.append(stmt)

        elif isinstance(stmt, ExecStmt):
            self.to_exec[line_num] = stmt
            self.flattened.extend(stmt.term.tree.flattened.keys())

        self.error_handler.remove_line(self.path)  # error was not raised

    def run(self):
        """Runs this session's executable statements by expanding them and then beta-reducing them. Will raise any
        errors that are encountered.
        """
        self._expand_scope()   # expand NamedFuncs in scope if new NamedFuncs have been added

        for stmt in self.memory:    # reduce/sub iff stmt is used in ExecStmts
            if stmt.name in self.flattened:
                stmt.sub_all(self.to_exec.values())

        for line_num, stmt in list(self.to_exec.items()):  # run ExecStmts
            if self.cmd_line:
                self.results = [stmt.execute()]
                del self.to_exec[line_num]
            else:
                self.results.append(stmt.execute())

    def pop(self):
        """Returns and removes last result. Used in command-line mode."""
        return self.results.pop().tree.expr

    def _get_paths(self, path):
        """Returns [path] if path != 'common' else absolute paths of common lc files."""
        if path == "common":
            return [os.path.abspath(f"{self.common_path}/{file}") for file in os.listdir(self.common_path)]
        return [os.path.abspath(path)]

    def _expand_scope(self):
        """Expands any NamedFuncs within self.scope and moves them to self.memory."""
        funcs, seen, move = {func.name: func for func in self.scope}, [], []
        for idx, func in enumerate(self.scope):
            for node, paths in func.term.flattened.items():
                if node in funcs and node not in seen:
                    raise GenericException("'{}' used prior to definition", node)
                elif node in funcs:
                    for path in paths:
                        funcs[node].sub(func, path)

            self.memory.append(func)
            move.append(idx)

            seen.append(func.name)

        for num, idx in enumerate(move):  # can't modify self.scope during iteration, so do it here
            del self.scope[idx - num]
