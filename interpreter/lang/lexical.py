"""Lexical analysis for lc language, a shallow wrapper around pure lambda calculus. Note that this module does not
provide input file parsing, but rather tokenization of arbitrary string expressions.

All grammar can be defined as follows:

```
<import_stmt> ::= "#import " <filepath>     ; imports relative to this .lc file
<define_stmt> ::= "#define " <char> <char>  ; blindly replaces instances of first <char> with second <char>

<named_func> ::= <var> ":=" <λ-term>        ; only reduced if used later on
<exec_stmt> ::= <λ-term>                    ; will be outputted when interpreter is run
```
"""

from abc import abstractmethod, ABC
from copy import deepcopy

from interpreter.lang.numerical import cnumberify, numberify
from interpreter.pure.lexical import LambdaTerm, Variable, NormalOrderReducer, PureGrammar


PureGrammar.illegal.append("#")   # character for signifying import statement
PureGrammar.illegal.append("\"")  # character that surrounds filepath in import statement
PureGrammar.illegal.append(":=")  # characters for declaring a named func/define statements


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
    def infer(cls, expr, defines=None):
        """Similar to LambdaTerm's generate_tree, this method infers the type of expr and returns an object of the
        correct grammar subclass. No *args, **kwargs support because any args besides expr are handled internally and
        should not be used. Defines represents #define substitutions to be made (default is None).
        """
        for stmt in defines:
            expr = stmt.replace(expr)

        for subclass_name in cls.__subclasses__():
            subclass = globals()[subclass_name.__name__]
            if subclass.check_grammar(expr):
                return subclass(expr)

        raise SyntaxError(f"'{expr}' is not valid lc grammar")

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

        __, path = expr.split(" ")
        self.path = path.strip("\"")

    @staticmethod
    def check_grammar(expr):
        if not expr.startswith("#import"):
            return False

        try:
            hash_import, path = expr.split(" ")

            assert hash_import == "#import"
            assert path.startswith("\"") and path.endswith("\"")

        except (AssertionError, ValueError):
            raise SyntaxError(f"'{expr}' has invalid import statement syntax")

        return True


class DefineStmt(Grammar):
    """Define statement in lc. See docstrings for grammar."""
    ALIASES = {"<lambda>": "λ", "<declare>": ":=", "<hash>": "#"}

    def __init__(self, expr):
        super().__init__(expr)

        __, self.to_replace, self.replacement = self.expr.split(" ")

    @staticmethod
    def check_grammar(expr):
        if not expr.startswith("#define"):
            return False

        try:
            hash_define, to_replace, replacement = expr.split(" ")

            assert hash_define == "#define"
            assert all(not char.isspace() for char in to_replace)
            assert all(not char.isspace() and char not in PureGrammar.illegal for char in replacement)

        except (AssertionError, ValueError):
            raise SyntaxError(f"'{expr}' has invalid define statement syntax")

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

    def __init__(self, expr):
        super().__init__(expr)

        name, term = self.expr.split(":=")
        self.name = Variable(name)
        self.term = NormalOrderReducer(term)
        cnumberify(self.term)

        if self.name in self.term.flattened:
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
