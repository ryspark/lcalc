"""Pure lambda calculus abstract syntax tree token generator and parser.

Formally, lambda calculus grammar can be succintly defined as
<LambdaExpr> ::= <LambdaVar>                        ; must be single character (by convention, lowercase)
               | "λ" <LambdaVar> "." <LambdaExpr>   ; this and the next definition are the only nonterminals
               | <LambdaExpr> <LambdaExpr>          ; associating by left: abcd = ((((a) b) c) d)

The `pure` directory contains pure lambda calculus AST generation and parsing- not sufficient for the lc language.

Source: https://opendsa-server.cs.vt.edu/ODSA/Books/PL/html/Syntax.html

"""

from abc import ABC, abstractmethod
import re


class Invariate:
    CHARS = {
        "<open_paren>": "(",
        "<close_paren>": ")",
        "<period>": ".",
        "<lambda>": "λ"
    }

    def __init__(self, char):
        assert char in Invariate.CHARS.values(), "{} not a valid invariate".format(char)
        self.char = char


class Grammar(ABC):

    @abstractmethod
    def __init__(self, expr, checked=False, *args, **kwargs):
        ...

    @staticmethod
    @abstractmethod
    def check_grammer(expr, *args, **kwargs):
        ...


class LambdaVar(Grammar):

    def __init__(self, expr, checked=False, scope=None):
        if checked or LambdaVar.check_grammar(expr, scope):
            self.char = expr
            self.scope = scope  # because it's mutable, it'll automatically update when scope updates
        else:
            raise ValueError("{} not proper LambdaVar grammar".format(expr))

    @staticmethod
    def check_grammar(expr, scope=None):
        # expr must be singular alphabetic character and not already defined in scope
        is_singular_alpha = expr.isalpha() and len(expr) == 1
        return is_singular_alpha and (not scope or expr not in scope)


class LambdaExpr(Grammar):

    def __init__(self, expr, checked=False):
        if checked or LambdaExpr.check_grammar(expr):
            self.bound = expr[1]  # character after <lambda>

            body = expr[expr.index(".")+1:]  # character after <period>
            if LambdaExpr.check_grammer(expr):
                self.body = LambdaExpr(body, checked=True)
            elif LambdaVar.check_grammer(expr):
                self.body = LambdaVar(body, checked=True)
            else:
                raise ValueError("{} not proper LambdaExpr or LambdaVar grammar".format(body))

        else:
            raise ValueError("{} not proper LambdaExpr grammar".format(expr))

    @staticmethod
    def check_grammar(expr):
        # format is λx.M, where x is an alphabet character and M is anything (will recursively checked)
        expr_format = re.compile("{}[^[a-zA-Z]$.*.".format(Invariate.CHARS["<lambda>"]))
        return expr_format.match(expr)


class LambdaAST: ...
