"""Pure lambda calculus abstract syntax tree token generator and parser.

The `pure` directory contains pure lambda calculus AST generation and parsing- not sufficient for the lc language.

Formally, pure lambda calculus can be defined as

<λ-term> ::= <λ-term>                   ; "variable"
                                        ; - must be alphabetic character(s) (by convention, lowercase)
           | "λ" <λ-term> "." <λ-term>  ; "abstraction"
                                        ; - currying is not supported in this implementation (*)
                                        ; - abstraction bodies are greedy: λx.x y = λx.(x y) != (λx.x) (y)
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

import abc

class Grammar(abc.ABC):
    """Superclass that represents any character/set of characters present in lambda calculus."""

    def __init__(self, expr, check=False):
        self.expr = Grammar.preprocess(expr)
        self._cls = type(self).__name__
        self.nodes = []

        if check and not self.check_grammar(self.expr):
            raise SyntaxError("'{}' not valid {} grammar".format(expr, self._cls))

    @staticmethod
    @abc.abstractmethod
    def check_grammar(expr):
        """"This method should check expr's top-level grammar and return whether or not it is valid. It should also
        raise a SyntaxError if expr's top-level grammar is similar to the accepted grammar but syntactically invalid."""

    @staticmethod
    def are_parens_balanced(expr):
        parens_balance = 0
        for idx, char in enumerate(expr):
            if parens_balance < 0:
                break
            elif char == "(":
                parens_balance += 1
            elif char == ")":
                parens_balance -= 1
        return parens_balance == 0

    @staticmethod
    def preprocess(original_expr, strip_parens=True):
        """Strips surrounding whitespace and outer parentheses (if any) from expr."""
        if not original_expr:
            raise SyntaxError("Lambda term cannot be empty")

        expr = original_expr.lstrip().rstrip()
        if strip_parens and expr[0] + expr[-1] == "()" and Grammar.are_parens_balanced(expr[1:-1]):
            expr = expr[1:-1]

        if original_expr == expr:
            return original_expr
        return Grammar.preprocess(expr, strip_parens)

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


class Builtin(Grammar):
    """Built-in lambda calculus tokens: 'λ', '.', '(', and ')'. """

    TOKENS = ("λ", ".", "(", ")")

    @staticmethod
    def check_grammar(expr):
        if len(expr) == 1:
            return expr in Builtin.TOKENS
        if all(char in Builtin.TOKENS for char in expr):
            raise SyntaxError("'{}' has stray builtins".format(expr))
        else:
            return False


class LambdaTerm(Grammar):
    """Represents a valid λ-term: variable, abstraction, or application."""

    @abc.abstractmethod
    def step_tokenize(self):
        """This method should tokenize a LambdaTerm only one level down. Assumes top-level grammar has been checked,
        but raises an error if second-level grammar is not valid. Should set and return self.nodes attribute.

        Examples: "(x y) (λx.λy.λz.x (y z))" -> [Application("x y"), Abstraction("λx.λy.λz.x (y z)")]
                  "λx.λy.λz.x (y z)" -> [Variable("x"), Abstraction("λy.λz.x (y z)")]

        Source: https://opendsa-server.cs.vt.edu/ODSA/Books/PL/html/Syntax.html
        """

    @staticmethod
    def infer_type(expr):
        """Converts expr to the proper LambdaTerm type, raises SyntaxError if expr is not a valid LambdaType."""
        for cls in LambdaTerm.__subclasses__():
            subclass = globals()[cls.__name__]
            if subclass.check_grammar(expr):
                return subclass(expr)
        raise SyntaxError("'{}' not valid LambdaTerm grammar".format(expr))


class Variable(LambdaTerm):
    """Variable in lambda calculus: alphabetic character(s) that represent abstractions."""

    @staticmethod
    def check_grammar(expr):
        """Variable format: <alpha> (alphabetic character(s))"""
        return Grammar.preprocess(expr).isalpha() and "λ" not in expr

    def step_tokenize(self):
        """Variables cannot be tokenized, so this method raises a TypeError."""
        raise TypeError("Variable object is not tokenizable")


class Abstraction(LambdaTerm):
    """Abstraction: the basic datatype in lambda calculus."""

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
        - Accepted Abstraction format: "λ" <Variable> "." <[^Builtin*]> (optional parentheses not shown)

        The reason for this discrepancy is to delegate recursive token parsing to LambdaAST.
        """
        expr = Grammar.preprocess(expr)

        # check 1: are required Builtins (λ, .) correctly placed and mathed within expr?
        bind = expr.find("λ")
        decl = expr.find(".")

        if bind == decl:
            return False
        elif expr.count("λ") != expr.count(".") or (bind != -1 and decl == -1) or (decl != -1 and bind == -1):
            raise SyntaxError("'{}' has mismatched binders/declarators".format(expr))
        elif decl < bind:
            raise SyntaxError("'{}' has declarator before binder".format(expr))
        elif bind != 0:
            return False

        # check 2: is bound variable valid?
        bound_var = Abstraction.get_bound_var(expr)
        if not Variable.check_grammar(bound_var):
            return False
        elif any(Builtin.check_grammar(char) for char in bound_var):
            raise SyntaxError("'{}' contains an illegal bound variable".format(expr))

        # check 3: is body valid?
        body = Abstraction.get_body(expr)
        if len(body) == 0 or all(Builtin.check_grammar(char) for char in body):
            raise SyntaxError("'{}' contains an illegal abstraction body".format(expr))
        return True

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
        expr = Grammar.preprocess(expr)

        # check 1: are parentheses balanced?
        if not Grammar.are_parens_balanced(expr):
            raise SyntaxError("'{}' has mismatched parentheses".format(expr))

        # check 2: are there spaces or parentheses in expr?
        if " " not in expr and ("(" not in expr or ")" not in expr):
            return False

        # check 3: is expr an Builtin, Variable, or Abstraction?
        is_builtin = Builtin.check_grammar(expr)
        is_other_lambdaterm = Variable.check_grammar(expr) or Abstraction.check_grammar(expr)
        return not is_builtin and not is_other_lambdaterm

    def step_tokenize(self):
        print("\n-> expr: " + self.expr)
        padded_expr = " " + Grammar.preprocess(self.expr) + " "

        split = []                       # chunks of self.expr
        open_pos, extras = None, 0       # for splitting by parentheses
        space_pos = None                 # for splitting by spaces
        in_body, bind_parens = False, 0  # for making sure that abstraction bodies are greedy

        # TODO: try to fix by using abstraction_levels instead of in_body, bin_parens, etc.

        for idx, char in enumerate(padded_expr):
            if padded_expr[idx] == "λ":
                # if "λ" found, assume that abstraction body has been entered
                in_body = True

            if char == "(":
                if in_body:
                    # if in abstraction declaration, increment parens balance counter for that body
                    bind_parens += 1
                elif open_pos is None:
                    # if not within nested parens and not within an abstraction body, set open_pos
                    open_pos = idx + 1
                    if space_pos:
                        # needed if there is not a space in between open_paren and last LambdaTerm
                        split.append(padded_expr[space_pos:idx])
                else:
                    # if in nested parens and not in abstraction body, increment balance counter
                    extras += 1
                space_pos = None

            elif char == ")":
                if in_body and bind_parens == 0:
                    # if exiting an abstraction body, turn in_body off
                    in_body = False
                if in_body:
                    # if in abstraction body, decrement abstraction body balance counter
                    bind_parens -= 1
                elif open_pos is not None and extras == 0:
                    # if not within nested parens and not within an abstraction body, add to split
                    split.append(padded_expr[open_pos:idx])
                    open_pos = None
                else:
                    # if within nested parens and not in abstraction body, decrement balance counter
                    extras -= 1

            elif char.isspace() and (not in_body or idx == len(padded_expr) - 1):
                if space_pos is not None:
                    # grab chunk in between this space and last one if previous space has been set
                    split.append(padded_expr[space_pos:idx])
                if open_pos is None:
                    # if not in nested parens, set space_pos
                    space_pos = idx + 1

            elif padded_expr[idx] == "λ":
                decl = padded_expr.find(".", idx)
                if decl == -1 or not Variable.check_grammar(padded_expr[idx + 1:decl]):
                    raise SyntaxError("'{}' has unmatched binder".format(padded_expr))

            elif padded_expr[idx] == ".":
                bind = padded_expr.rfind("λ", 0, idx)
                if bind == -1 or not Variable.check_grammar(padded_expr[bind + 1:idx]):
                    raise SyntaxError("'{}' has unmatched declarator".format(padded_expr))

            if extras < 0 or bind_parens < 0:
                raise SyntaxError("'{}' has mismatched parentheses".format(self.expr))

            print("idx: {}, char: {}, open_pos: {}, extras: {}, space_pos: {}, in_body: {}, bind_parens: {}".format(
                idx, char, open_pos, extras, space_pos, in_body, bind_parens
            ))

        print(split)

        for chunk in split:
            if chunk == self.expr:
                raise SyntaxError("'{}' not valid Application grammar".format(self.expr))
            elif chunk != "":
                self.nodes.append(LambdaTerm.infer_type(chunk))

        return self.nodes

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

if __name__ == "__main__":
    print(LambdaTerm.infer_type("(((x)) λx.)"))
    import time
    s = time.time()
    print(LambdaAST("x((λx.x) λx.x) (λxy.(λx.(y x)))"))
    print(time.time() - s)
