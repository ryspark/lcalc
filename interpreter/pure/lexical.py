"""Pure lambda calculus abstract syntax tree token generator and parser.

The `pure` directory contains pure lambda calculus AST generation and parsing- not sufficient for the lc language.

Formally, pure lambda calculus can be defined as

<λ-term> ::= <λ-term>                   ; "variable"
                                        ; - must be alphabetic character(s) (by convention, lowercase)
           | "λ" <λ-term> "." <λ-term>  ; "abstraction"
                                        ; - currying is not supported in this implementation (*)
           | <λ-term> <λ-term>          ; "application"
                                        ; - associating by left: abcd = ((((a) b) c) d)

Note that applications and abstractions are the only non-terminals in lambda calculus.

Sources: https://plato.stanford.edu/entries/lambda-calculus/#Com

------------------------------------------------------------------------------------------------------------------------

(*) Why is currying not supported? Because it makes the use of multi-character Variable ambiguous. For example, if
currying is allowed, what does the expression `λint.x` mean? Should it be resolved to `λi.λn.λt.x`, or is `int` a
Variable name? Thus, currying and multi-character Variable cannot coexist without causing ambiguity. This
implementation favors multi-character LambdaVars over currying, a purely arbitrary decision. Relatedly, this feature
means that function application must be separated by spaces or parentheses.

"""

from abc import ABC, abstractmethod
import re

class LambdaTerm(ABC):
    """Superclass that represents any character/set of characters present in lambda calculus."""

    def __init__(self, expr, check=False):
        self._cls = type(self).__name__
        if check:
            assert self.check_grammar(expr), "'{}' not valid {} grammar".format(expr, self._cls)
        self.expr = expr
        self.nodes = []

    @staticmethod
    @abstractmethod
    def check_grammar(expr):
        """"This method should check expr's top-level grammar and return whether or not it is valid."""
        pass

    def step_tokenize(self):
        """This method should tokenize a LambdaTerm only one level down. Assumes top-level grammar has been checked,
        but raises an error if second-level grammar is not valid. Should set self.nodes attrs but also return tokenized.

        Examples: "(x y) (λx.λy.λz.x (y z))" -> [Application("x y"), Abstraction("λx.λy.λz.x (y z)")]
                  "λx.λy.λz.x (y z)" -> [Variable("x"), Abstraction("λy.λz.x (y z)")]

        If not overriden, this method assumes the subclass is not tokenizable any further and raises a ValueError.

        Source: https://opendsa-server.cs.vt.edu/ODSA/Books/PL/html/Syntax.html
        """
        raise ValueError("{} object is not tokenizable".format(self._cls))

    @staticmethod
    def infer_type(expr):
        """Converts expr to the proper LambdaTerm type"""
        for cls in LambdaTerm.__subclasses__():
            subclass = globals()[cls.__name__]
            if subclass.check_grammar(expr):
                return subclass(expr)
        raise ValueError("'{}' not valid LambdaTerm grammar".format(expr))

    def display(self, indents=0):
        nodes = "{}{}(expr='{}'".format("    " * (indents if indents <= 1 else indents // 2), self._cls, self.expr)
        if self.nodes:
            nodes += ", nodes=["
            for node in self.nodes:
                nodes += "\n{}".format("    " * indents) + node.display(indents + 1) + ","
            nodes += "\n{}]".format("    " * indents)
        
        return nodes + ")"

    def __str__(self):
        return self.__repr__()

    def __repr__(self):
        return "{}('{}')".format(self._cls, self.expr)

    def __eq__(self, other):
        return hasattr(other, "expr") and self.expr == other.expr


class Builtin(LambdaTerm):
    """Builtins represent the 'keywords' of lambda calculus. Not technically LambdaTerms,
    but it's convenient to subclass."""

    @staticmethod
    def check_grammar(expr):
        return len(expr) == 1 and expr in ("λ", ".", "(", ")")

class Variable(LambdaTerm):
    """Variable in lambda calculus: alphabetic character(s) that represent abstractions."""

    @staticmethod
    def check_grammar(expr):
        """Variable format: <alpha> (alphabetic character(s))"""
        return re.match(r"^[a-zA-Z]+$", expr) is not None


class Abstraction(LambdaTerm):
    """Abstraction: the only datatype in lambda calculus, assuming variables represent abstractions."""

    @staticmethod
    def get_bound_var(expr):
        """Assumes grammar check has been run."""
        return expr[expr.index("λ") + 1:expr.index(".")]

    @staticmethod
    def get_body(expr):
        """Assumes grammar check has been run."""
        return expr[expr.index(".") + 1:]

    @staticmethod
    def check_grammar(expr):
        """This implementation of Abstraction differs from the actual lambda calculus definition.
        - Actual Abstraction format: "λ" <Variable> "." <LambdaTerm> (optional parentheses not shown)
        - Accepted Abstraction format: "λ" <Variable> "." <^Application> (optional parentheses not shown)

        The reason for this discrepancy is to delegate recursive token parsing to LambdaAST.

        """
        # check 1: are required Builtinss (λ, .) correctly placed within expr?
        if not expr.find("λ") == 0 or "." not in expr:
            return False

        # check 2: is bound variable valid?
        bound_var = Abstraction.get_bound_var(expr)
        if not Variable.check_grammar(bound_var):
            return False

        # check 3: is expr an Application in disguise? (ex: λx.x λy.y)
        body = Abstraction.get_body(expr)
        return "λ" not in body or body.index("λ") == 0

    def step_tokenize(self):
        bound_var = Variable(self.get_bound_var(self.expr))
        body = LambdaTerm.infer_type(self.get_body(self.expr))

        self.nodes = [Builtin("λ"), bound_var, Builtin("."), body]
        return self.nodes


class Application(LambdaTerm):
    """Application of arbitrary number of Abstractions/Variables."""

    @staticmethod
    def check_grammar(expr):
        """This implementation of Application differs from the actual lambda calculus definition.
        - Actual Application format: <LambdaTerm>+ (grouped by parentheses and spaces)
        - Accepted Application format: anything but other LambdaTerms, grouped correctly by parens/spaces

        """
        # check 1: is expr an Builtins, Variable, or Abstraction?
        if Builtin.check_grammar(expr) or Variable.check_grammar(expr) or Abstraction.check_grammar(expr):
            return False

        # check 2: are parentheses balanced?
        paren_balance = 0
        for char in expr:
            if paren_balance < 0:
                return False
            elif char == "(":
                paren_balance += 1
            elif char == ")":
                paren_balance -= 1
        return paren_balance == 0

    def step_tokenize(self):
        raise NotImplementedError()

class LambdaAST:
    """Implements a syntax tree builder as well as normal-order beta reduction of that syntax tree.

    Sources: https://opendsa-server.cs.vt.edu/ODSA/Books/PL/html/Syntax.html,
             https://opendsa-server.cs.vt.edu/ODSA/Books/PL/html/ReductionStrategies.html
    """

    def __init__(self, expr):
        self.expr = expr
        self.tree = LambdaTerm.infer_type(expr)
        LambdaAST.generate_tree(self.tree)

    @staticmethod
    def generate_tree(node):
        if not isinstance(node, Variable) and not isinstance(node, Builtin):
            for sub_node in node.step_tokenize():
                LambdaAST.generate_tree(sub_node)

    def __repr__(self):
        return self.tree.__repr__()

    def __str__(self):
        return self.tree.display()
