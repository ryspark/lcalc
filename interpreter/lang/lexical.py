"""Lexical analysis for lcalc language, a shallow wrapper around pure lambda calculus. Note that this module does not
provide input file parsing, but rather tokenization of arbitrary string expressions.

All grammar can be loosely defined as follows:

```
<import_stmt> ::= "#import " <filepath>     ; imports relative to this .lc file ("common" denotes common/*.lc)
<define_stmt> ::= "#define " <char> <char>  ; blindly replaces instances of first <char> with second <char>
                                            ; note that there are a few special cases for the second <char>:
                                            ;  1. <lambda> will replace all λ with first <char>
                                            ;  2. <declare> will replace all := with first <char>

<named_func>  ::= <var> ":=" <λ-term>       ; only reduced if used later on
<exec_stmt>   ::= <λ-term>                  ; will be outputted when interpreter is run

<comment>     ::= ";;" <char>*
```

Comments are handled in session.py: there is no dedicated Grammar class for comments.
"""

from abc import abstractmethod, ABC
from copy import deepcopy

from lang.error import GenericException
from lang.numerical import cnumberify, numberify
from pure.lexical import Abstraction, Application, LambdaTerm, Variable, NormalOrderReducer, PureGrammar


PureGrammar.illegal.append(";;")   # characters for signifying beginning of comment
PureGrammar.illegal.append("#")    # character for signifying import statement
PureGrammar.illegal.append("\"")   # character that surrounds filepath in import statement
PureGrammar.illegal.append(":=")   # characters for declaring a named func/define statements

PureGrammar.illegal.append("<lambda>")   # '#define' representation for lambda (λ) character
PureGrammar.illegal.append("<declare>")  # '#define' representation for declare (:=) character


class Grammar(ABC):
    """Superclass representing any grammar object in lcalc language."""

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
        """This method should check expr's top-level grammar and return whether or not it is valid. It should also
        raise a SyntaxError if expr's top-level grammar is similar to the accepted grammar but syntactically invalid.
        original_expr is used for error messages.
        """

    @staticmethod
    def preprocess(expr):
        """Removes trailing whitespace."""
        return expr.rstrip()

    @classmethod
    def infer(cls, expr, original_expr=None):
        """Similar to LambdaTerm's generate_tree, this method infers the type of expr and returns an object of the
        correct grammar subclass.
        """

        def _infer(cls, expr, original_expr):
            for subclass_name in cls.__subclasses__():
                subclass = globals()[subclass_name.__name__]
                if subclass.__subclasses__():
                    return _infer(subclass_name, expr, original_expr)
                elif subclass.check_grammar(expr, original_expr):
                    return subclass(expr, original_expr)

            raise GenericException("'{}' is not valid lcalc grammar", original_expr)

        return _infer(cls, expr, original_expr if original_expr else expr)

    def __repr__(self):
        return f"{self._cls}('{self.expr}')"

    def __str__(self):
        return self.expr

    def __eq__(self, other):
        return isinstance(other, type(self)) and other.expr == self.expr

    def __hash__(self):
        return hash(self.expr)


class ImportStmt(Grammar):
    """Import statement in lcalc. See docstrings for grammar."""

    def __init__(self, expr, original_expr=None):
        super().__init__(expr, original_expr)

        __, *path = self.expr.split(" ")
        self.path = Grammar.preprocess("".join(path))[1:-1]  # get rid of surrounding " " and whitespace

    @staticmethod
    def check_grammar(expr, original_expr):
        expr = Grammar.preprocess(expr)

        if not expr.startswith("#import"):
            return False

        try:
            hash_import, *path = expr.split(" ")
            path = Grammar.preprocess("".join(path))

            assert hash_import == "#import"
            assert path.startswith("\"") and path.endswith("\"")

        except (AssertionError, ValueError):
            raise GenericException("#import expects \"FILENAME\"")

        return True


class DefineStmt(Grammar):
    """#define statement in lcalc. See docstrings for grammar."""
    ALIASES = {"<lambda>": "λ", "<declare>": ":="}

    def __init__(self, expr, original_expr):
        super().__init__(expr, original_expr)

        __, *to_replace, self.replacement = self.expr.split(" ")
        self.to_replace = Grammar.preprocess("".join(to_replace))

    @staticmethod
    def check_grammar(expr, original_expr):
        expr = Grammar.preprocess(expr)

        if not expr.startswith("#define"):
            return False

        try:
            hash_define, *__, replacement = expr.split(" ")

            assert hash_define == "#define"
            assert not any(phrase in replacement and phrase not in DefineStmt.ALIASES for phrase in PureGrammar.illegal)

        except (AssertionError, ValueError):
            raise GenericException("#define expects REPLACEMENT TO_REPLACE")

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


class FuncStmt(Grammar):
    """Superclass for function statements (executable or named) in lcalc."""
    term: NormalOrderReducer
    namespace: dict

    def _check_namespace(self):
        """If named func is used as bound abstraction variable, this method raises a GenericException. Called during
        register_namespace.
        """
        for node_expr in self.namespace:
            for path in self.flattened().get(node_expr, [[]])[-1]:
                if isinstance(self.term.get(path[:-1]), Abstraction) and path[-1] == 0:
                    msg = "'{}' contains bound variable already defined in namespace"
                    start = self.original_expr.index(f"λ{node_expr}") + 1
                    raise GenericException(msg, self.original_expr, start=start, end=start + len(node_expr))

    def register_namespace(self, namespace):
        """Registers and checks self.term with namespace. Also expands self.term with namespace."""
        self.namespace = namespace
        self._check_namespace()

        for node_expr, (__, paths) in self.flattened().items():
            if node_expr in self.namespace:
                self.namespace[node_expr].sub_paths(self, paths)

    def flattened(self, recompute=False):
        """Flattened Variables. Used for substitution in Session."""
        return self.term.flattened(recompute)


class NamedFunc(FuncStmt):
    """NamedFuncs represent binding statements in lcalc: <NAME> := <λ-term>."""

    def __init__(self, expr, original_expr):
        super().__init__(expr, original_expr)

        name, term = self.expr.split(":=")
        term = term.strip()

        self.name = Variable(name, term)
        self.term = NormalOrderReducer(term)
        cnumberify(self.term)

        if self.name.expr in self.flattened():
            raise GenericException("recursive definitions not supported", self.term)

    @staticmethod
    def check_grammar(expr, original_expr):
        expr = Grammar.preprocess(expr)

        # check 1: is ":=" in expr?
        eq = expr.find(":=")
        if eq == -1:
            return False
        elif eq != expr.rfind(":="):
            start = original_expr.index(expr) + expr.rfind(":=")
            raise GenericException("'{}' contains illegal reserved ':='", original_expr, start=start, end=start + 2)

        lval, rval = expr.split(":=")

        # check 2: is l-value a Variable/number?
        if LambdaTerm.infer_type(lval, original_expr) is not Variable:
            msg = "l-value of '{}' is not a valid variable"
            raise GenericException(msg, original_expr, end=original_expr.index(":="))

        elif PureGrammar.preprocess(lval, original_expr).isdigit():
            msg = "l-value of '{}' is a real number"
            raise GenericException(msg, original_expr, end=original_expr.index(":="))

        # check 3: is r-value a LambdaTerm?
        if not LambdaTerm.infer_type(rval, original_expr):
            msg = "r-value of '{}' is not a valid LambdaTerm"
            raise GenericException(msg, original_expr, start=original_expr.index(rval))

        return True

    def sub_paths(self, func_stmt, paths):
        """Substitutes self.term.tree for every path in paths in func_stmt."""
        for path in paths:
            func_stmt.term.set(path, deepcopy(self.term.tree))

    def rsub(self, term, recompute):
        """Reverse-substitutes any occurence of self.term in term for self.name. Returns whether or not flattened needs
        to be recomputed.
        """
        to_recompute = False
        for node_expr, (node, paths) in term.flattened(recompute).items():
            if self.term.tree.alpha_equals(node):
                to_recompute = True
                for path in paths:
                    term.set(path, self.name)
        return to_recompute

    def __repr__(self):
        return f"{self._cls}(name={repr(self.name)}, replace={repr(self.term)})"


class ExecStmt(FuncStmt):
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

    def execute(self, error_handler, sub):
        """Running an ExecStmt is equivalent to beta-reducing its term. If sub, this method will reverse substitutes
        names of NamedStmts back into self.term after reducing it.
        """
        self.term.beta_reduce(error_handler)
        numberify(self.term)

        if sub and self.namespace:
            recompute = True
            for named_stmt in self.namespace.values():
                recompute = named_stmt.rsub(self.term, recompute)

        return self.term.tree.expr
