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

Sources: https://plato.stanford.edu/entries/lambda-calculus/#Com,
         https://opendsa-server.cs.vt.edu/ODSA/Books/PL/html/Syntax.html,
         http://pages.cs.wisc.edu/~horwitz/CS704-NOTES/1.LAMBDA-CALCULUS.html#NOR

------------------------------------------------------------------------------------------------------------------------

(*) Why is currying not supported? Because it makes the use of multi-character Variable ambiguous. For example, if
currying is allowed, what does the expression `λvar.x` mean? Should it be resolved to `λv.λa.λr.x`, or is `var` a
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
    def check_grammar(expr):
        """"This method should check expr's top-level grammar and return whether or not it is valid. It should also
        raise a SyntaxError if expr's top-level grammar is similar to the accepted grammar but syntactically invalid."""

    @staticmethod
    def are_parens_balanced(expr):
        """Checks if parenthese are balanced within expr."""
        parens_balance = 0
        for char in expr:
            if parens_balance < 0:
                break
            elif char == "(":
                parens_balance += 1
            elif char == ")":
                parens_balance -= 1
        return parens_balance == 0

    @staticmethod
    def preprocess(original_expr):
        """Strips surrounding whitespace and outer parentheses (if any) from expr."""
        if not original_expr:
            raise SyntaxError("Lambda term cannot be empty")

        for char in Grammar.illegal:
            if char in original_expr:
                raise SyntaxError("'{}' contains illegal character {}".format(original_expr, char))

        expr = original_expr.lstrip().rstrip()
        if expr[0] + expr[-1] == "()" and Grammar.are_parens_balanced(expr[1:-1]):
            expr = expr[1:-1]

        if original_expr == expr:
            return original_expr
        return Grammar.preprocess(expr)

    def display(self, indents=0):
        """Recursively displays Grammar tree with readable format.

        Format:
        <Grammar>(expr='<expr>', nodes=[
            <Grammar>(expr='<expr>', nodes=[
                ...
                <Grammar>(expr='<expr>')  # <-- if nodes is empty
            ])
        ])
        """
        result = "{}{}(expr='{}'".format("    " * indents, self._cls, self.expr)
        if self.nodes:
            result += ", nodes=["
            for node in self.nodes:
                result += "\n" + node.display(indents + 1) + ","
            result = result[:-1] + "\n{}]".format("    " * indents)
        return result + ")"

    def __repr__(self):
        return "{}('{}')".format(self._cls, self.expr)

    def __eq__(self, other):
        if isinstance(other, Grammar):
            return all(var == other_var for var, other_var in zip(vars(self), vars(other)))
        return False


class Builtin(Grammar):
    """Built-in lambda calculus tokens: 'λ', '.', '(', ')', ' '. """
    TOKENS = ["λ", ".", "(", ")", " "]

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
        """

    @abc.abstractmethod
    def substitute(self, var, new_arg):
        """Equivalent to self.expr[var := new_arg] or self.expr[new_arg/var]."""

    @property
    @abc.abstractmethod
    def tokenizable(self):
        """Whether or not this object is tokenizable. Could make everything work without this method, but felt that it
        was more explicit (and therefore worth the few extra lines of code) to have a node.tokenizable check in
        LambdaAST.generate_tree."""

    @property
    @abc.abstractmethod
    def is_leftmost(self):
        """Whether or not this object is a leftmost node in a syntax tree. Will only work if step_tokenize has been
        run."""

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
        raise TypeError("Variable object is not tokenizable")

    def substitute(self, var, new_arg):
        """Variable substitution follows two rules:
        1. self.expr[var := new_arg] = new_arg, self.expr == var
        2. self.expr[var := new_arg] = self.expr, self.expr != var
        """
        if self.expr == var:
            self.expr = new_arg

    @property
    def tokenizable(self):
        return False

    @property
    def is_leftmost(self):
        return False


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
        - Accepted Abstraction format: "λ" <Variable> "." <anything> (optional parentheses not shown)

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
        bound_var = Variable(Abstraction.get_bound_var(self.expr))
        body = LambdaTerm.infer_type(Abstraction.get_body(self.expr))

        self.nodes = [bound_var, body]

    def substitute(self, var, new_arg):
        """Abstraction substitution follows two rules:
        1. (λvar.body)[var := new_arg] = λvar.body
        2. (λ!var.body)[var := new_arg] = (λ!var.body[var := new_arg])
        """
        bound_var, body = self.nodes
        if var != bound_var:
            body.substitute(var, new_arg)

    @property
    def tokenizable(self):
        return True

    @property
    def is_leftmost(self):
        return False


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
        padded_expr = " " + self.expr + " "

        split = []                            # chunks of self.expr
        open_pos, extras = None, 0            # for splitting by parentheses
        space_pos = None                      # for splitting by spaces
        bind_positions, bind_parens = [], []  # for making sure that abstractions are greedy

        for idx, char in enumerate(padded_expr):
            previous_char = padded_expr[idx - 1] if idx > 0 else None

            if char == "λ" and open_pos is None:
                # if "λ" is found and within nested parens, mark start of abstraction
                bind_positions.append(idx)
                bind_parens.append(0)

                if previous_char not in Builtin.TOKENS:
                    # so that xλ.x is illegal but (x)λx.x, λx.(λy.y), and λx.λy.y aren't
                    raise SyntaxError("'{}' has illegal character before bind".format(self.expr))

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

            elif previous_char == ")" and not char.isspace():
                # if previous character is ')', mark this character as start of new expr
                space_pos = idx

            elif char.isspace() and (not bind_positions or idx == len(padded_expr) - 1):
                if bind_positions:
                    # if bind_positions, idx must be at the end of padded_expr
                    # therefore, if there are any unclosed abstractions, add them to split
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

    def substitute(self, var, new_arg):
        """Application substitution is distributive: (A B)[x := M] = (A[x := M])(B[x := M])"""
        for node in self.nodes:
            node.substitute(var, new_arg)

    @property
    def tokenizable(self):
        return True

    @property
    def is_leftmost(self):
        """Applications are the only LambdaTerm that can be leftmost. An Application is leftmost if its left child is
        an Abstraction."""
        try:
            return isinstance(self.nodes[0], Abstraction)
        except IndexError:
            raise ValueError("[internal] call step_tokenize before querying nodes")


class LambdaAST:
    """Implements a syntax tree builder as well as normal-order beta reduction of that syntax tree."""

    def __init__(self, expr):
        self.expr = expr
        self.tree = LambdaTerm.infer_type(expr)
        LambdaAST.generate_tree(self.tree)

    @staticmethod
    def generate_tree(node):
        """In-place recursive generation of syntax tree from a single LambdaTerm object. No unit test for this method,
        mostly because it's annoying to write one. However, assuming step_tokenize and tokenizable work, the only part
        of this method that could possibly go wrong is the recursion, so no need for a test here."""
        if node.tokenizable:
            node.step_tokenize()
            for sub_node in node.nodes:
                LambdaAST.generate_tree(sub_node)

    def left_outer_redex(self):
        """Returns the leftmost outermost redex, if there is one. The first outermost node is returned without checking
        if it is leftmost, but generally first outermost node == leftmost outermost node."""

        def outer_redex(tree, candidates):
            if tree.is_leftmost:
                candidates.append(tree)
            else:
                candidates.extend(outer_redex(node, candidates) for node in tree.nodes)

        candidates = []
        outer_redex(self.tree, candidates)

        try:
            return next(filter(lambda node: node, candidates))
        except StopIteration:
            return None

    def __repr__(self):
        return repr(self.tree)

    def __str__(self):
        return self.tree.display()


if __name__ == "__main__":
    import time
    times = []

    for _ in range(100):
        start = time.time()
        syntax_tree = LambdaAST("(λz.((λy.z (v y)) λy.(y v)) λv.z)")
        syntax_tree.left_outer_redex()
        times.append(time.time() - start)

    print(sum(times) / len(times))

    print(LambdaAST("(λx.x) (λx.x) (λx.x) (λx.x)"))
