"""Pure lambda calculus abstract syntax tree token generator and parser.

Formally, pure lambda calculus grammar can be succintly defined as
<LambdaExpr> ::= <LambdaVar>                        ; called a variable (is a LambdaTerm)
                                                    ; - must be alphabetic character(s) (by convention, lowercase)
               | "λ" <LambdaVar> "." <LambdaExpr>   ; called an abstraction (is a LambdaTerm)
                                                    ; - currying is NOT allowed in this implementation (*)
               | <LambdaExpr> <LambdaExpr>          ; called an application (is a LambdaTerm)
                                                    ; - associating by left: abcd = ((((a) b) c) d)

Note that LambdaVar and LambdaExpr are the only non-terminals in lambda calculus.

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

####################################### Invariants #######################################
INVARIATES = {
    "<bind>": "λ",  # binds variables to Abstraction
    "<decl>": ".",  # precedes Abstraction body/declaration
    "<open>": "(",
    "<close>": ")",
}


def invariates():
    return INVARIATES.values()


####################################### Lambda calculus syntax #######################################
class Nonterminal(ABC):

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


class LambdaVar(Nonterminal):

    def __init__(self, expr, check=False):
        if check:
            assert LambdaVar.check_grammar(expr), "'{}' not proper LambdaVar grammar".format(expr)
        self.name = expr

    @staticmethod
    def check_grammar(expr):
        # checked/actual LambdaVar format is: <alpha> (must be alphabetic character, no other requirements)
        return expr.isalpha()


class LambdaExpr(Nonterminal):

    def __init__(self, expr, check=False):
        if check:
            assert LambdaExpr.check_grammar(expr), "'{}' not proper LambdaExpr grammar".format(expr)

        self.bound_vars = LambdaExpr.get_bound_vars(expr)
        self.body = LambdaExpr.get_body(expr, add_parens=True)

        bind, decl, __, __ = invariates()
        self.expr = bind + self.bound_vars + decl + self.body

    @staticmethod
    def get_bound_vars(expr):
        # assumes first LambdaExpr check has been run on expr
        return expr[1:expr.index(INVARIATES["<decl>"])]

    @staticmethod
    def get_body(expr, add_parens):
        # assume that check_grammar has validated expr
        __, __, open_paren, close_paren = invariates()

        # get body of LambdaExpr with format "λ" <LambdaVar> "." <LambdaTerm>
        bind_idx = expr.index(INVARIATES["<bind>"])
        body = expr[bind_idx + 1:]

        if add_parens:
            # if expr is enclosed in parentheses, make sure they match; otherwise, add parenthese
            if body[0] == open_paren:
                assert body[-1] == close_paren, "'{}' parentheses do not match".format(expr)
            else:
                body = open_paren + body + close_paren

        return body

    @staticmethod
    def check_grammar(expr):
        # checked LambdaExpr format is "λ" <LambdaVar> "." anything (anything will be recursively checked)
        # actual LambdaExpr format is "λ" <LambdaVar> "." <LambdaExpr> (not checked until LambdaAST)

        bind = INVARIATES["<bind>"]

        # first check: are required invariates (<lambda>, .) in expr?
        if not (bind in expr and expr.index(bind) == 0) or not "." in expr:
            return False

        # second check: is bound variable list valid?
        if not LambdaExpr.get_bound_vars(expr).isalpha():
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
        bind, decl, open_paren, close_paren = invariates()
        split = []

        start, extras = None, 0
        for idx, char in enumerate(expr):
            if char == open_paren:
                if start is None:
                    start = idx + 1  # expr[idx] is open_paren, so don't include that
                else:
                    extras += 1

            elif char == close_paren:
                if extras == 0 and start:
                    split.append(expr[start:idx])
                    start, extras = None, 0
                else:
                    extras -= 1

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


if __name__ == "__main__":
    print(LambdaAST("((z) (λx.λy.z)) ((x) (y))"))
