"""Lexical analysis for lc language, a shallow wrapper around pure lambda calculus. Note that this module does not
provide input file parsing, but rather tokenization of arbitrary string expressions.

PureGrammar (not sure if this is done properly...):

```
<named_func> ::= <var> ":=" <λ-term>
<exec_stmt> ::= <λ-term>

<import_stmt> ::= "#import " " <filepath> "\""  ; relative to this .lc file
```

"""

from abc import abstractmethod, ABC
from copy import deepcopy
import re

from interpreter.lang.numerical import cnumberify, numberify
from interpreter.pure.lexical import LambdaTerm, Variable, NormalOrderReducer, PureGrammar


PureGrammar.illegal.append("#")   # character for signifying import statement
PureGrammar.illegal.append("\"")  # character that surrounds filepath in import statement
PureGrammar.illegal.append(":=")  # characters for declaring a named func


class Grammar(ABC):
    """Superclass representing any grammar object in lc language."""

    def __init__(self, expr):
        """Assumes check_grammar has been run."""
        self.expr = expr
        self._cls = type(self).__name__

    @staticmethod
    @abstractmethod
    def check_grammar(expr):
        """"This method should check expr's top-level grammar and return whether or not it is valid. It should also
        raise a SyntaxError if expr's top-level grammar is similar to the accepted grammar but syntactically invalid.
        """

    @classmethod
    def infer(cls, expr):
        """Similar to LambdaTerm's generate_tree, this method infers the type of expr and returns an object of the
        correct grammar subclass. No *args, **kwargs support because any args besides expr are handled internally and
        should not be used.
        """

        def _infer(cls, expr):
            for subclass_name in cls.__subclasses__():
                subclass = globals()[subclass_name.__name__]
                if subclass.__subclasses__():
                    return _infer(subclass, expr)
                elif subclass.check_grammar(expr):
                    return subclass(expr)

            raise SyntaxError(f"'{expr}' is not valid lc grammar")

        return _infer(cls, expr)

    def __repr__(self):
        return f"{self._cls}('{self.expr}')"

    def __eq__(self, other):
        return isinstance(other, type(self)) and other.expr == self.expr

    def __hash__(self):
        return hash(self.expr)


class ImportStmt(Grammar):
    """Import statement in lc. See docstrings for grammar."""

    def __init__(self, expr):
        super().__init__(expr)

        self.path = re.match("\"(.*)\"", expr).group()

    @staticmethod
    def check_grammar(expr):
        has_hash_sign = expr.startswith("#")
        if not has_hash_sign:
            return False

        if re.match("#import \".*\"", expr) is None:
            raise SyntaxError(f"'{expr}' has invalid import stmt syntax")

        return True


class FuncObj(Grammar):
    """Thin wrapper around NormalOrderReducer used to represent LambdaTerm objects in the lc language."""

    def __init__(self, expr):
        self.original_expr = expr
        self._cls = type(self).__name__
        self.term = None  # placeholder so property methods don't complain

    @property
    def expr(self):
        """Returns expr as given by NormalOrderReducer."""
        return self.term.tree.expr

    @property
    def flattened(self):
        """Returns flattened as given by NormalOrderReducer."""
        return self.term.flattened


class NamedFunc(FuncObj):

    def __init__(self, expr):
        super().__init__(expr)

        name, term = self.original_expr.split(":=")
        self.name = Variable(name)
        self.term = NormalOrderReducer(term)
        cnumberify(self.term)

        if self.name in self.flattened:
            raise SyntaxError("recursion not supported in lambda calculus")

    @staticmethod
    def check_grammar(expr):
        # check 1: is ":=" in expr?
        eq = expr.find(":=")
        if eq == -1:
            return False
        elif eq != expr.rfind(":="):
            raise SyntaxError(f"'{expr}' contains stray ':='")

        lval, rval = expr.split(":=")

        # check 2: is r-value a LambdaTerm?
        if not LambdaTerm.infer_type(rval):
            raise SyntaxError(f"r-value of '{expr}' is not a valid LambdaTerm")

        # check 3: is l-value a Variable?
        if LambdaTerm.infer_type(lval) is not Variable:
            raise SyntaxError(f"l-value of '{expr}' is not a valid Variable")
        elif PureGrammar.preprocess(lval).isdigit():
            raise SyntaxError(f"l-value of '{expr}' is a real number")

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
            for node, paths in func_obj.flattened.items():
                if node == self.name:
                    for path in paths:
                        func_obj.term.set(path, deepcopy(self.term.tree))

    def sub_all(self, to_exec):
        """In-place beta-reduction and substitution of elements of to_exec."""
        for stmt in to_exec:
            self.sub(stmt)

    def __repr__(self):
        return f"{self._cls}(name={repr(self.name)}, replace={repr(self.term)})"


class ExecStmt(FuncObj):
    """Another thin wrapper around NormalOrderReducer, which provides functionality for directly executing LambdaTerm
    statements.
    """

    def __init__(self, expr):
        super().__init__(expr)
        self.term = NormalOrderReducer(expr)
        cnumberify(self.term)

    @staticmethod
    def check_grammar(expr):
        return LambdaTerm.infer_type(expr) is not None

    def execute(self):
        """Running an ExecStmt is equivalent to beta-reducing its term."""
        self.term.beta_reduce()
        numberify(self.term)
        return self.term
