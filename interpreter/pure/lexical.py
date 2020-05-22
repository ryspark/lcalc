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
from dataclasses import dataclass
import re

####################################### Invariates #######################################
@dataclass(init=False)
class Invariate:
    char: str

    def __init__(self, char):
        assert char in ("λ", ".", "(", ")"), "{} not an invariate".format(char)
        self.char = char

    def display(self, indents=0):
        return "    " * (indents if indents <= 1 else indents // 2) + self.__str__()


####################################### Lambda calculus syntax #######################################
class LambdaTerm(ABC):

    def __init__(self, expr, check=False):
        if check:
            assert self.check_grammar(expr), "'{}' not valid {} grammar".format(expr, type(self).__name__)
        self.expr = expr
        self.nodes = []

    @staticmethod
    @abstractmethod
    def check_grammar(expr):
        """"This method should check expr's top-level grammar and return whether or not it is valid."""
        pass

    @abstractmethod
    def step_tokenize(self):
        """This method should tokenize a LambdaTerm only one level down. Assumes top-level grammar has been checked,
        but raises an error if second-level grammar is not valid. Should set self.nodes attrs but also return tokenized.

        Examples: "(x y) (λx.λy.λz.x (y z))" -> [Application("x y"), Abstraction("λx.λy.λz.x (y z)")]
                  "λx.λy.λz.x (y z)" -> [Variable("x"), Abstraction("λy.λz.x (y z)")]

        Source: https://opendsa-server.cs.vt.edu/ODSA/Books/PL/html/Syntax.html
        """
        pass

    @staticmethod
    def infer_type(expr):
        """Converts expr to the proper LambdaTerm type"""
        for cls in LambdaTerm.__subclasses__():
            subclass = globals()[cls.__name__]
            if subclass.check_grammar(expr):
                return subclass(expr)
        raise ValueError("'{}' not valid LambdaTerm grammar".format(expr))

    def display(self, indents=0):
        name_indents = indents if indents <= 1 else indents // 2
        nodes = "{}{}(expr='{}'".format("    " * name_indents, type(self).__name__, self.expr)
        
        if self.nodes:
            nodes += ", nodes=["
            for node in self.nodes:
                nodes += "\n{}".format("    " * indents) + node.display(indents + 1) + ","
            nodes += "\n{}]".format("    " * indents)
        
        return nodes + ")"

    def __str__(self):
        return self.__repr__()

    def __repr__(self):
        return "{}('{}')".format(type(self).__name__, self.expr)

    def __eq__(self, other):
        return hasattr(other, "expr") and self.expr == other.expr


class Variable(LambdaTerm):

    @staticmethod
    def strip(expr):
        return expr.replace("(", "").replace(")", "")

    @staticmethod
    def check_grammar(expr):
        """Accepted/actual Variable format: <alpha> (alphabetic character(s), possibly with parentheses)"""
        # check 1: is expr valid alphabetic character(s)?
        return Variable.strip(expr).isalpha() and "λ" not in expr

    def step_tokenize(self):
        raise ValueError("Variable object is a terminal")


class Abstraction(LambdaTerm):

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
        # check 1: are required invariates (λ, .) correctly placed within expr?
        if not expr.find("λ") == 0 or "." not in expr:
            return False

        # check 2: is bound variable valid?
        if not Variable.check_grammar(expr[1]):
            return False

        # check 3: is it an Application in disguise? (ex: λx.x λy.y)
        body = Abstraction.get_body(expr)
        return "λ" not in body or body.index("λ") == 0

    def step_tokenize(self):
        bound_var = Variable(self.expr[1])
        body = LambdaTerm.infer_type(self.expr[self.expr.index(".") + 1:])

        self.nodes = [Invariate("λ"), bound_var, Invariate("."), body]
        return self.nodes


class Application(LambdaTerm):

    @staticmethod
    def check_grammar(expr):
        """This implementation of Application differs from the actual lambda calculus definition.
        - Actual Application format: <LambdaTerm>+ (grouped by parentheses and spaces)
        - Accepted Application format: <^[Invariate, Variable, Abstraction]> (grouped by parentheses and spaces)

        The reason for this discrepancy is twofold: to delegate recursive token parsing to LambdaAST,
        but also to comply with definition of Abstraction (see check 3 in Abstraction)

        """
        # check 1: do parentheses match?
        if expr.count("(") != expr.count(")"):
            return False

        # check 2: is it not a valid Variable or Abstraction?
        if Variable.check_grammar(expr) or Abstraction.check_grammar(expr):
            return False

        # check 3: are parentheses syntactically correct?
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
        # 1. find the top-level matching parentheses
        parens_idxs = [0]  # needed for part 2
        start, extras = None, 0

        for idx, char in enumerate(self.expr):
            if char == "(":
                if start is None:
                    start = idx + 1  # expr[idx] == (, so don't include that
                else:
                    extras += 1

            elif char == ")":
                if extras == 0 and start:
                    parens_idxs.extend((start, idx))
                    start, extras = None, 0
                else:
                    extras -= 1

        # 2. split using the results from part 1 and by spaces
        parens_idxs.append(len(self.expr))  # needed to make sure that the whole expr gets parsed

        split = []
        for num in range(len(parens_idxs) - 1):
            chunk = self.expr[parens_idxs[num]:parens_idxs[num + 1]]
            if num == 0 or num % 2 == 0:
                split.extend(filter(lambda expr: expr not in ("", "(", ")"), chunk.split(" ")))
            else:
                split.append(chunk)

        self.nodes = list(map(LambdaTerm.infer_type, split))
        return self.nodes


####################################### Lambda calculus syntax tree #######################################
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
        if not isinstance(node, Variable) and not isinstance(node, Invariate):
            for sub_node in node.step_tokenize():
                LambdaAST.generate_tree(sub_node)

    def __repr__(self):
        return self.tree.__repr__()

    def __str__(self):
        return self.tree.display()
