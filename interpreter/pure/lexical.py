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

####################################### Lambda calculus syntax #######################################
class LambdaTerm(ABC):

    def __init__(self, expr, check=False):
        if check:
            assert self.check_grammar(expr), "'{}' not valid {} grammar".format(expr, type(self).__name__)
        self.expr = expr

    @staticmethod
    @abstractmethod
    def check_grammar(expr): ...

    def __repr__(self):
        attrs = "{}(".format(type(self).__name__)
        for name, val in self.__dict__.items():
            attrs += "{}='{}', ".format(name, val)
        return attrs.rstrip(", ") + ")"

    def __str__(self):
        return self.__repr__()


class Variable(LambdaTerm):

    @staticmethod
    def check_grammar(expr):
        """Accepted/actual Variable format: <alpha> (alphabetic character, possibly with parentheses)"""
        expr = expr.replace("(", "").replace(")", "")
        return expr.isalpha() and len(expr) == 1 and "λ" not in expr


class Abstraction(LambdaTerm):

    @staticmethod
    def check_grammar(expr):
        """This implementation of Abstraction differs from the actual lambda calculus definition.
        - Actual Abstraction format: "λ" <Variable> "." <LambdaTerm> (optional parentheses not shown)
        - Accepted Abstraction format: "λ" <Variable> "." <Variable>+ (optional parentheses not shown)

        The reason for this discrepancy is to delegate recursive token parsing to LambdaAST.

        """

        # check 1: are required invariates (λ, .) in expr?
        if not ("λ" in expr and expr.index("λ") == 0) or not "." in expr:
            return False

        # check 2: is bound variable list valid?
        if not expr[1:expr.index(".")].isalpha():
            return False

        # check 3: is the definition a valid Application or Variable?
        return Variable.check_grammar(expr) or Application.check_grammar(expr)


class Application(LambdaTerm):

    @staticmethod
    def check_grammar(expr):
        """This implementation of Application differs from the actual lambda calculus definition.
        - Actual Application format: <LambdaTerm> <LambdaTerm>
        - Accepted Application format: <Abstraction> <Abstraction> | <Variable> <Variable>

        The reason for this discrepancy is twofold: to delegate recursive token parsing to LambdaAST,
        but also to comply with definition of Abstraction (see check 3 in Abstraction)

        """
        pass


####################################### Lambda calculus syntax tree #######################################
class LambdaAST:
    """Implements a syntax tree builder as well as normal-order beta reduction of that syntax tree.

    Sources: https://opendsa-server.cs.vt.edu/ODSA/Books/PL/html/Syntax.html,
             https://opendsa-server.cs.vt.edu/ODSA/Books/PL/html/ReductionStrategies.html
    """

    def __init__(self, expr):
        self.expr = expr
        self.tree = []

    @staticmethod
    def step_tokenize(expr):
        """Tokenzies expr only one level down.

        For example, `a λx.λy.z x y` gets tokenized into ["a", "λx.y.z x y"], while `(λx.y.z.z (x y)) b` gets
        tokenized into ["λx.y.z.z (x y)", "b"].

        Source: https://opendsa-server.cs.vt.edu/ODSA/Books/PL/html/Syntax.html
        """

        # 0. make sure parentheses match
        assert expr.count("(") == expr.count(")"), "parentheses mismatch in {}".format(expr)

        # 1. find the top-level matching parentheses
        parens_idxs = []
        start, extras = None, 0

        for idx, char in enumerate(expr):
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
        parens_idxs.insert(0, 0)
        parens_idxs.append(len(expr))
        # above two lines needed to make sure that the whole expr gets parsed

        split = []
        for num in range(len(parens_idxs) - 1):
            chunk = expr[parens_idxs[num]:parens_idxs[num + 1]]
            if num == 0 or num % 2 == 0:
                split.extend(filter(lambda char: char not in ("(", ")", ""), chunk.split(" ")))
            else:
                split.append(chunk)

        return split

    def parse_token(self, expr):
        # implemented with recursion principles, but without actually using recursion

        if Variable.check_grammar(expr):
            # "base case" 1: expr is a valid Variable
            return Variable(expr)
        elif Abstraction.check_grammar(expr):
            # "base case" 2: expr is a valid Abstraction
            return Abstraction(expr)
        elif Application.check_grammar(expr):
            # "base case" 3: expr is valid Application
            return Application(expr)
        else:
            # "recursion" otherwise
            return LambdaAST.step_tokenize(expr)

    def beta_reduce(self):
        raise NotImplementedError()

    def __repr__(self):
        return str(self.tree)

    def __str__(self):
        return self.__repr__()


if __name__ == "__main__":
    print(LambdaAST("((λx.x) λx.x) (λxy.y λabc.a) λxy.y"))
