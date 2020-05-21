"""Pure lambda calculus abstract syntax tree token generator and parser.

Formally, pure lambda calculus grammar can be succintly defined as
<LambdaExpr> ::= <LambdaVar>                        ; called a variable (is a LambdaTerm)
                                                    ; - must be alphabetic character(s) (by convention, lowercase)
               | "λ" <LambdaVar> "." <LambdaExpr>   ; called an abstraction (is a LambdaTerm)
                                                    ; - currying is NOT allowed in this implementation (*)
               | <LambdaExpr> <LambdaExpr>          ; called an application (is a LambdaTerm)
                                                    ; - associating by left: abcd = ((((a) b) c) d)

Note that applications and abstractions are the only non-terminals in lambda calculus.

The `pure` directory contains pure lambda calculus AST generation and parsing- not sufficient for the lc language.

Sources: https://opendsa-server.cs.vt.edu/ODSA/Books/PL/html/Syntax.html,
         https://plato.stanford.edu/entries/lambda-calculus/#Com

------------------------------------------------------------------------------------------------------------------------

(*) Why disallow currying? Because it makes the use of multi-character LambdaVars ambiguous.

For example, if currying is allowed, what does the expression `λint.x` mean?

Should it evaluate to `λi.λn.λt.x` or is `int` a LambdVar? Thus, currying and multi-character LambdaVars cannot
coexist without causing ambiguity. This implementation favors multi-character LambdaVars over currying, a purely
arbitrary decision.

Relatedly, this feature means that function application must be separated by spaces or parentheses

"""

from abc import ABC, abstractmethod

####################################### Lambda calculus syntax #######################################
class LambdaTerm(ABC):

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


class LambdaVar(LambdaTerm):

    def __init__(self, expr, check=False):
        if check:
            assert LambdaVar.check_grammar(expr), "'{}' not proper LambdaVar grammar".format(expr)
        self.name = expr

    @staticmethod
    def check_grammar(expr):
        # checked/actual LambdaVar format is: <alpha> (must be alphabetic character, no other requirements)
        return expr.isalpha() and "λ" not in expr


class LambdaExpr(LambdaTerm):

    def __init__(self, expr, check=False):
        if check:
            assert LambdaExpr.check_grammar(expr), "'{}' not proper LambdaExpr grammar".format(expr)

        self.expr = expr
        self.bound_vars = expr[1:expr.index(".")]
        self.body = expr[expr.index("λ") + 1:]

    @staticmethod
    def check_grammar(expr):
        # checked LambdaExpr format is "λ" <LambdaVar> "." anything (anything will be recursively checked)
        # actual LambdaExpr format is "λ" <LambdaVar> "." <LambdaExpr> (not checked until LambdaAST)

        # first check: are required invariates (λ, .) in expr?
        if not ("λ" in expr and expr.index("λ") == 0) or not "." in expr:
            return False

        # second check: is bound variable list valid?
        if not expr[1:expr.index(".")].isalpha():
            return False

        # expr matches above checked format
        return True


####################################### Lambda calculus syntax tree #######################################
class LambdaAST:
    """Implements a syntax tree builder as well as normal-order beta reduction of that syntax tree.

    Sources: https://opendsa-server.cs.vt.edu/ODSA/Books/PL/html/Syntax.html,
             https://opendsa-server.cs.vt.edu/ODSA/Books/PL/html/ReductionStrategies.html
    """

    def __init__(self, expr):
        self.expr = expr
        self.tree = []
        self.generate_tree(expr)

    @staticmethod
    def tokenize(expr):
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
            chunk = expr[parens_idxs[num]:parens_idxs[num+1]]
            if num == 0 or num % 2 == 0:
                split.extend(filter(lambda char: char not in ("(", ")", ""), chunk.split(" ")))
            else:
                split.append(chunk)

        return split

    def generate_tree(self, expr):
        # -- base cases:

        # 1. expr is a valid Variable
        if LambdaVar.check_grammar(expr):
            self.tree.append(LambdaVar(expr))

        # 2. expr is a valid Abstraction
        elif LambdaExpr.check_grammar(expr):
            self.tree.append(LambdaExpr(expr))

        # -- recursion otherwise
        else:
            for token in LambdaAST.tokenize(expr):
                self.generate_tree(token)

    def beta_reduce(self):
        raise NotImplementedError()

    def __repr__(self):
        return str(self.tree)

    def __str__(self):
        return self.__repr__()


if __name__ == "__main__": ...
