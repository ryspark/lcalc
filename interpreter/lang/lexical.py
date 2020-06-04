"""Lexical analysis for lc language, a shallow wrapper around pure lambda calculus. Note that this module does not
provide input file parsing, but rather tokenization of arbitrary string expressions.

All grammar can be loosely defined as follows:

```
<import_stmt> ::= "#import " <filepath>     ; imports relative to this .lc file ("common" denotes common/*.lc)
<define_stmt> ::= "#define " <char> <char>  ; blindly replaces instances of first <char> with second <char>

<named_func> ::= <var> ":=" <λ-term>        ; only reduced if used later on
<exec_stmt> ::= <λ-term>                    ; will be outputted when interpreter is run

<comment> ::= "--" <char>*
```

Comments are handled in session.py: there is no dedicated Grammar class for comments.
"""

from abc import abstractmethod, ABC
from copy import deepcopy

from lang.error import template
from lang.numerical import cnumberify, numberify
from pure.lexical import Application, LambdaTerm, Variable, NormalOrderReducer, PureGrammar


PureGrammar.illegal.append("--")   # characters for signifying beginning of comment
PureGrammar.illegal.append("#")    # character for signifying import statement
PureGrammar.illegal.append("\"")   # character that surrounds filepath in import statement
PureGrammar.illegal.append(":=")   # characters for declaring a named func/define statements


class Grammar(ABC):
    """Superclass representing any grammar object in lc language."""

    def __init__(self, expr, original_expr=None):
        """Assumes check_grammar has been run."""
        if original_expr is None:
            original_expr = Grammar.preprocess(expr)

        self.expr = Grammar.preprocess(expr)
        self.original_expr = original_expr  # used for errors messages
        self._cls = type(self).__name__

    @staticmethod
    @abstractmethod
    def check_grammar(expr, original_expr):
        """"This method should check expr's top-level grammar and return whether or not it is valid. It should also
        raise a SyntaxError if expr's top-level grammar is similar to the accepted grammar but syntactically invalid.
        original_expr is used for error messages.
        """

    @staticmethod
    def preprocess(expr):
        """Removes trailing whitespace."""
        return expr.rstrip()

    @classmethod
    def infer(cls, expr, original_expr):
        """Similar to LambdaTerm's generate_tree, this method infers the type of expr and returns an object of the
        correct grammar subclass. No *args, **kwargs support because any args besides expr are handled internally and
        should not be used.
        """
        for subclass_name in cls.__subclasses__():
            subclass = globals()[subclass_name.__name__]
            if subclass.check_grammar(expr, original_expr):
                return subclass(expr, original_expr)
        raise ValueError(template("'{}' is not valid lc grammar", original_expr))

    def __repr__(self):
        return f"{self._cls}('{self.expr}')"

    def __eq__(self, other):
        return isinstance(other, type(self)) and other.expr == self.expr

    def __hash__(self):
        return hash(self.expr)


class ImportStmt(Grammar):
    """Import statement in lc. See docstrings for grammar."""

    def __init__(self, expr, original_expr=None):
        super().__init__(expr, original_expr)

        __, path = self.expr.split(" ")
        self.path = path[1:-1]  # get rid of surrounding " "

    @staticmethod
    def check_grammar(expr, original_expr):
        expr = Grammar.preprocess(expr)

        if not expr.startswith("#import"):
            return False

        try:
            hash_import, path = expr.split(" ")

            assert hash_import == "#import"
            assert path.startswith("\"") and path.endswith("\"")

        except (AssertionError, ValueError):
            raise SyntaxError(template("#import expects \"FILENAME\""))

        return True


class DefineStmt(Grammar):
    """Define statement in lc. See docstrings for grammar."""
    ALIASES = {"<lambda>": "λ", "<declare>": ":=", "<hash>": "#"}

    def __init__(self, expr, original_expr):
        super().__init__(expr, original_expr)

        __, self.to_replace, self.replacement = self.expr.split(" ")

    @staticmethod
    def check_grammar(expr, original_expr):
        expr = Grammar.preprocess(expr)

        if not expr.startswith("#define"):
            return False

        try:
            hash_define, to_replace, replacement = expr.split(" ")

            assert hash_define == "#define"
            assert not any(char.isspace() for char in to_replace)
            assert not any(char.isspace() for char in replacement)
            assert not any(char in replacement for char in PureGrammar.illegal)

        except (AssertionError, ValueError):
            raise SyntaxError(template("#define expects REPLACEMENT TO_REPLACE"))

        return True

    def replace(self, expr):
        """Replaces all occurences of self.to_replace with self.replacement in expr. Also handles special aliases."""
        replacement = DefineStmt.ALIASES.get(self.replacement, self.replacement)
        to_replace = self.to_replace

        if replacement == "λ" and len(to_replace) > 1:
            # if to_replace is more than one character, assume extra space
            # ex: if to_replace == "lambda" and replacement is "λ", "lambda x" should be replaced, not "lambdax"
            to_replace += " "

        return expr.replace(to_replace, replacement)


class NamedFunc(Grammar):

    def __init__(self, expr, original_expr):
        super().__init__(expr, original_expr)

        name, term = self.expr.split(":=")
        self.name = Variable(name, original_expr)
        self.term = NormalOrderReducer(term)
        cnumberify(self.term)

        if self.name in self.term.flattened:
            raise SyntaxError(template("recursive definitions not supported", self.term))

    @staticmethod
    def check_grammar(expr, original_expr):
        expr = Grammar.preprocess(expr)

        # check 1: is ":=" in expr?
        eq = expr.find(":=")
        if eq == -1:
            return False
        elif eq != expr.rfind(":="):
            start = original_expr.rfind(":=")
            raise SyntaxError(template("'{}' contains stray ':='", original_expr, start=start, end=start + 2))

        lval, rval = expr.split(":=")

        # check 2: is l-value a Variable/number?
        if LambdaTerm.infer_type(lval, original_expr) is not Variable:
            msg = "l-value of '{}' is not a valid Variable"
            raise SyntaxError(template(msg, original_expr, end=original_expr.index(":=")))

        elif PureGrammar.preprocess(lval, original_expr).isdigit():
            msg = "l-value of '{}' is a real number"
            raise SyntaxError(template(msg, original_expr, end=original_expr.index(":=")))

        # check 3: is r-value a LambdaTerm?
        if not LambdaTerm.infer_type(rval, original_expr):
            msg = "r-value of '{}' is not a valid LambdaTerm"
            raise SyntaxError(template(msg, original_expr, start=original_expr.index(rval)))

        return True

    def sub(self, func_obj, precalc_path=None):
        """Substitutes all occurences of self.name in tree for self.replace. tree must be a FuncObj. If precalc_path is
        specified, sub will simply call func_obj.term.set(precalc_path, self.term.term.tree).
        """
        if not self.term.reduced:
            self.term.beta_reduce()

        if precalc_path is not None:
            func_obj.term.set(precalc_path, self.term.tree)
        else:
            for node, paths in func_obj.term.flattened.items():
                if node == self.name:
                    for path in paths:
                        func_obj.term.set(path, deepcopy(self.term.tree))

    def sub_all(self, to_exec):
        """In-place beta-reduction and substitution of elements of to_exec."""
        for stmt in to_exec:
            self.sub(stmt)

    def __repr__(self):
        return f"{self._cls}(name={repr(self.name)}, replace={repr(self.term)})"


class ExecStmt(Grammar):
    """Another thin wrapper around NormalOrderReducer, which provides functionality for directly executing LambdaTerm
    statements.
    """

    def __init__(self, expr, original_expr):
        super().__init__(expr, original_expr)
        self.term = NormalOrderReducer(expr, original_expr)
        cnumberify(self.term)

    @staticmethod
    def check_grammar(expr, original_expr):
        return LambdaTerm.infer_type(expr, original_expr) is not None

    def execute(self):
        """Running an ExecStmt is equivalent to beta-reducing its term."""
        self.term.beta_reduce()
        numberify(self.term)
        return self.term
