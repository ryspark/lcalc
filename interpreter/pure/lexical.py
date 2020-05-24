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
    illegal = []

    def __init__(self, expr, check=False):
        self.expr = Grammar.preprocess(expr)
        self._cls = type(self).__name__
        self.nodes = []

        if check and not self.check_grammar(self.expr):
            raise SyntaxError("'{}' not valid {} grammar".format(expr, self._cls))

    @staticmethod
    @abc.abstractmethod
    def tokenizable():
        """Whether or not this Grammar object is tokenizable. Could make everything work without this method, but
        felt that it was more explicit (and therefore worth the few extra lines of code) to have a node.tokenizable()
        call in LambdaAST.generate_tree."""

    @staticmethod
    @abc.abstractmethod
    def check_grammar(expr):
        """"This method should check expr's top-level grammar and return whether or not it is valid. It should also
        raise a SyntaxError if expr's top-level grammar is similar to the accepted grammar but syntactically invalid."""

    @staticmethod
    def are_parens_balanced(expr):
        """Checks if parenthese are balanced within expr."""
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

        for char in Grammar.illegal:
            if char in original_expr:
                raise SyntaxError("'{}' contains illegal character {}".format(original_expr, char))

        expr = original_expr.lstrip().rstrip()
        if strip_parens and expr[0] + expr[-1] == "()" and Grammar.are_parens_balanced(expr[1:-1]):
            expr = expr[1:-1]

        if original_expr == expr:
            return original_expr
        return Grammar.preprocess(expr, strip_parens)

    def display(self, _indents=0):
        """Recursively displays Grammar tree with readable format.

        Format:
        <Grammar>(expr='<expr>', nodes=[
            <Grammar>(expr='<expr>', nodes=[
                ...
                <Grammar>(expr='<expr>')  # <-- if nodes is empty
            ])
        ])
        """
        nodes = "{}{}(expr='{}'".format("    " * (_indents if _indents <= 1 else _indents // 2), self._cls, self.expr)
        if self.nodes:
            nodes += ", nodes=["
            for node in self.nodes:
                nodes += "\n{}".format("    " * _indents) + node.display(_indents + 1) + ","
            nodes += "\n{}]".format("    " * _indents)

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
    def tokenizable():
        return False

    @staticmethod
    def check_grammar(expr):
        if len(expr) == 1:
            return expr in Builtin.TOKENS
        if all(char in Builtin.TOKENS for char in expr):
            raise SyntaxError("'{}' has stray builtins".format(expr))
        return False


class LambdaTerm(Grammar):
    """Represents a valid λ-term: variable, abstraction, or application."""

    @abc.abstractmethod
    def step_tokenize(self):
        """This method should tokenize a LambdaTerm only one level down. Assumes top-level grammar has been checked,
        but raises an error if second-level grammar is not valid. Should set self.nodes attribute.

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
    def tokenizable():
        return False

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
    def tokenizable():
        return True

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

        # check 1: are required Builtins (λ, .) correctly placed and matched within expr?
        bind = expr.find("λ")
        decl = expr.find(".")

        if bind == decl:
            return False
        elif expr.count("λ") != expr.count(".") or (bind != -1 and decl == -1) or (decl != -1 and bind == -1):
            raise SyntaxError("'{}' has mismatched binds/declarators".format(expr))
        elif decl < bind:
            raise SyntaxError("'{}' has declarator before bind".format(expr))
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


class Application(LambdaTerm):
    """Application of arbitrary number of Abstractions/Variables."""

    @staticmethod
    def tokenizable():
        return True

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
        padded_expr = " " + self.expr + " "

        split = []                            # chunks of self.expr
        open_pos, extras = None, 0            # for splitting by parentheses
        space_pos = None                      # for splitting by spaces
        bind_positions, bind_parens = [], []  # for making sure that abstractions are greedy

        for idx, char in enumerate(padded_expr):
            if char == "λ" and open_pos is None:
                # if "λ" is found and within nested parens, mark start of λ-term
                bind_positions.append(idx)
                bind_parens.append(0)

            elif char == "." and open_pos is None:
                # if within top-level parens...
                if not bind_positions:
                    # if no "λ" have been recorded but char == ".", raise SyntaxError
                    raise SyntaxError("'{}' has declarator before bind".format(self.expr))
                if not Variable.check_grammar(padded_expr[bind_positions[-1] + 1:idx]):
                    # if chars between "." and last "λ" are not valid, raise SyntaxError
                    raise SyntaxError("'{}' has an illegal bound variable".format(self.expr))

            elif char == "(":
                if bind_positions:
                    # if in abstraction, increment parens balance counter for that specific abstraction
                    bind_parens[-1] += 1
                elif open_pos is None:
                    # if not within nested parens and not within an abstraction, set open_pos
                    open_pos = idx + 1
                    if space_pos:
                        # needed if there is not a space in between open_paren and last LambdaTerm
                        split.append(padded_expr[space_pos:idx])
                else:
                    # if in nested parens and not in abstraction, increment balance counter
                    extras += 1
                space_pos = None

            elif char == ")":
                if bind_positions and bind_parens[-1] == 0:
                    # if exiting an abstraction, remove last_bind from bind_positions/bind_parens
                    del bind_positions[-1]
                    del bind_parens[-1]
                if bind_positions:
                    # if in abstraction, decrement that abstraction body balance counter
                    bind_parens[-1] -= 1
                elif open_pos is not None and extras == 0:
                    # if not within nested parens and not within an abstraction body, add to split
                    split.append(padded_expr[open_pos:idx])
                    open_pos = None
                else:
                    # if within nested parens and not in abstraction body, decrement balance counter
                    extras -= 1

            elif char.isspace() and (not bind_positions or idx == len(padded_expr) - 1):
                if bind_positions:
                    # if bind_positions, idx must be at the end of padded_expr
                    # therefore, if there are any unclosed abstractions, add them to spli
                    space_pos = bind_positions[0]
                if space_pos is not None:
                    # grab chunk in between this space and last one if previous space has been set
                    split.append(padded_expr[space_pos:idx])
                if open_pos is None:
                    # if not in nested parens, set space_pos
                    space_pos = idx + 1

            if extras < 0 or (bind_parens and bind_parens[-1] < 0):
                raise SyntaxError("'{}' has mismatched parentheses".format(self.expr))

        for chunk in split:
            if chunk == self.expr:
                # if self.expr tokenizes to itself, it is invalid (Applications are always tokenizable)
                raise SyntaxError("'{}' not valid Application grammar".format(self.expr))
            elif chunk != "":
                self.nodes.append(LambdaTerm.infer_type(chunk))


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
        if node.tokenizable():
            node.step_tokenize()
            for sub_node in node.nodes:
                LambdaAST.generate_tree(sub_node)

    def __repr__(self):
        return self.tree.__repr__()

    def __str__(self):
        return self.tree.display()
