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

from abc import abstractmethod, ABC
from copy import copy, deepcopy
from string import ascii_letters


class Grammar(ABC):
    """Superclass that represents any character/set of characters present in lambda calculus."""
    illegal = []
    SUBS = ["\u2080", "\u2081", "\u2082", "\u2083", "\u2084", "\u2085", "\u2086", "\u2087", "\u2088", "\u2089"]

    def __init__(self, expr):
        """Does not provide grammar check: call check_grammar manually before instantiating Grammar obj."""
        self.expr = Grammar.preprocess(expr)
        self._cls = type(self).__name__
        self.nodes = []

    @staticmethod
    @abstractmethod
    def check_grammar(expr):
        """"This method should check expr's top-level grammar and return whether or not it is valid. It should also
        raise a SyntaxError if expr's top-level grammar is similar to the accepted grammar but syntactically invalid.
        """

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
                raise SyntaxError(f"'{original_expr}' contains illegal character '{char}'")

        expr = original_expr.lstrip().rstrip()
        if expr[0] + expr[-1] == "()" and Grammar.are_parens_balanced(expr[1:-1]):
            expr = expr[1:-1]

        if original_expr == expr:
            return original_expr
        return Grammar.preprocess(expr)

    @staticmethod
    def subscript(var, n):
        """Returns var with subscript of n."""
        subscripted = var
        for digit in str(n):
            subscripted += Grammar.SUBS[int(digit)]
        return subscripted

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
        result = f"{'    ' * indents}{self._cls}(expr='{self.expr}'"
        if self.nodes:
            result += ", nodes=["
            for node in self.nodes:
                result += "\n" + node.display(indents + 1) + ","
            result = result[:-1] + f"\n{'    ' * indents}]"
        return result + ")"

    def __repr__(self):
        return f"{self._cls}('{self.expr}')"

    def __eq__(self, other):
        if isinstance(other, Grammar):
            return all(attr == other_attr for attr, other_attr in zip(vars(self).values(), vars(other).values()))
        return False


class Builtin(Grammar):
    """Built-in lambda calculus tokens: 'λ', '.', '(', ')' """
    TOKENS = ["λ", ".", "(", ")"]

    @staticmethod
    def check_grammar(expr):
        if len(expr) == 1:
            return expr in Builtin.TOKENS
        if all(char in Builtin.TOKENS for char in expr):
            raise SyntaxError(f"'{expr}' has stray builtins")
        return False


class LambdaTerm(Grammar):
    """Represents a valid λ-term: variable, abstraction, or application. Also abstractly defines functionality that
    will allow a syntax tree to be built.
    """

    def __init__(self, expr):
        """In-place recursive generation of syntax tree from a single LambdaTerm object. No unit test for this method,
        mostly because it's annoying to write one. However, assuming tokenize and tokenizable work, the only part
        of this method that could possibly go wrong is the recursion, so no need for a test here.
        """
        super().__init__(expr)
        self.bound = []

        self.tokenize()
        self.generate_bound()

    @abstractmethod
    def tokenize(self):
        """This method should recursively tokenize a LambdaTerm. Assumes top-level grammar has been checked, but raises
        an error if second-level grammar is not valid. Should set self.nodes attribute, and will always generate two
        nodes (lambda calculus ASTs are binary), even for Applications.
        """

    @abstractmethod
    def alpha_convert(self, var, new_arg):
        """Given an abstraction λvar.M, this method renames all free occurences of var in M to new_arg. Proper usage of
        this method is provided in NormalOrderReducer. Assumes var is a Variable and new_arg a LambdaTerm, and should
        not update self.expr with alpha-converted expr.
        """

    @abstractmethod
    def beta_reduce(self, var, new_term):
        """Given a redex (λvar.M)new_term, this method substitutes all free occurences of x replaced by new_term. As
        with alpha_convert, this method is used in NormalOrderReducer.
        """

    @abstractmethod
    def generate_bound(self):
        """Generates bound variable list and sets self.bound attr. Assumes tokenize has been called."""

    @abstractmethod
    def update_expr(self):
        """Updates self.expr from self.nodes. Used after alpha_convert but not by it."""

    @property
    @abstractmethod
    def tokenizable(self):
        """Whether or not this object is tokenizable. Could make everything work without this method, but felt that it
        was more explicit (and therefore worth the few extra lines of code) to have a node.tokenizable check in
        LambdaAST.generate_tree.
        """

    @property
    @abstractmethod
    def is_leftmost(self):
        """Whether or not this object is a leftmost node in a syntax tree. Only works if tokenize has been run."""

    @staticmethod
    def generate_tree(expr):
        """Converts expr to the proper LambdaTerm type, raises SyntaxError if expr is not a valid LambdaType."""
        if not isinstance(expr, str):
            expr = "".join(char for char in expr)
        for cls in LambdaTerm.__subclasses__():
            subclass = globals()[cls.__name__]
            if subclass.check_grammar(expr):
                return subclass(expr)
        raise SyntaxError(f"'{expr}' not valid LambdaTerm grammar")

    def left_outer_redex(self):
        """Returns the leftmost outermost redex, if there is one. The first outermost node is returned without checking
        if it is leftmost, but generally first outermost node == leftmost outermost node.
        """

        def find_outer_redex(tree):
            if tree.is_leftmost:
                return tree
            try:
                return next(filter(lambda node: node, (find_outer_redex(node) for node in tree.nodes)))
            except StopIteration:
                return None

        return find_outer_redex(self)

    def check_args(self, var, new_arg, mode):
        """Checks alpha/beta reduction args."""
        if not isinstance(var, Variable) or not isinstance(new_arg, Variable if mode == "alpha" else LambdaTerm):
            raise ValueError("both var and new_arg must be of type Variable")
        if not isinstance(self, Variable) and not self.nodes:
            raise ValueError("call tokenize before using check_args")
        if new_arg in self.bound and mode == "alpha":
            raise ValueError(f"'{new_arg.expr}' is bound in '{self.expr}'")

    def get_new_arg(self, var, new_term):
        """Returns the next term that isn't var nor arg and occurs in neither body nor new_term."""
        already_used = [var]
        for node in self.nodes + [new_term]:
            if node.tokenizable:
                already_used.append(node)
            else:
                already_used.extend(node.bound)

        subscript = 0
        while True:
            new_arg = Grammar.subscript(new_term.expr, subscript)
            if new_arg not in already_used:
                return Variable(new_arg)
            subscript += 1

    def propagate_bound(self):
        """Propagates self.bound one level down. Called from generate_bound."""
        for node in self.nodes:
            if node.tokenizable:
                node.bound += self.bound

    def __str__(self):
        return self.display()
    
    def __copy__(self):
        """Used in beta-reduction to ensure that nodes do not get duplicated, leading to unexpected results."""
        return type(self)(self.expr)


class Variable(LambdaTerm):
    """Variable in lambda calculus: alphabetic character(s) that represent abstractions."""
    VALID = list(ascii_letters) + Grammar.SUBS

    @staticmethod
    def check_grammar(expr):
        """Variable format: <alpha> (alphabetic character(s))."""
        return all(char in Variable.VALID for char in Grammar.preprocess(expr))

    def tokenize(self):
        """Variables are not tokenizable, so do nothing."""

    def alpha_convert(self, var, new_arg):
        self.check_args(var, new_arg, mode="alpha")
        if self.expr == var.expr:
            self.expr = new_arg.expr

    def beta_reduce(self, var, new_term):
        """Beta conversion is the same as alpha conversion for Variables."""
        self.check_args(var, new_term, mode="beta")
        if self.expr == var.expr:
            self.expr = new_term.expr
            return copy(new_term)
        return self

    def generate_bound(self):
        """Variables have no nodes and therefore no bound variables."""

    def update_expr(self):
        """Variables have no nodes, so do nothing."""

    @property
    def tokenizable(self):
        return False

    @property
    def is_leftmost(self):
        return False


class Abstraction(LambdaTerm):
    """Abstraction: the basic datatype in lambda calculus."""

    @staticmethod
    def get_arg(expr):
        """Gets string repr of Abstraction arg from expr. Assumes grammar check has been run."""
        return expr[expr.index("λ") + 1:expr.index(".")]

    @staticmethod
    def get_body(expr):
        """Gets string repr of Abstraction body from expr. Assumes grammar check has been run."""
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
            raise SyntaxError(f"'{expr}' has mismatched binds/declarators")
        elif decl < bind:
            raise SyntaxError(f"'{expr}' has declarator before bind")
        elif bind != 0:
            return False

        # check 2: is bound variable valid?
        arg = Abstraction.get_arg(expr)
        if not Variable.check_grammar(arg):
            return False
        elif any(Builtin.check_grammar(char) for char in arg):
            raise SyntaxError(f"'{expr}' contains an illegal bound variable")

        # check 3: is body valid?
        body = Abstraction.get_body(expr)
        if len(body) == 0 or all(Builtin.check_grammar(char) for char in body):
            raise SyntaxError(f"'{expr}' contains an illegal abstraction body")
        return True

    def tokenize(self):
        arg = Variable(Abstraction.get_arg(self.expr))
        body = LambdaTerm.generate_tree(Abstraction.get_body(self.expr))

        self.nodes = [arg, body]

    def alpha_convert(self, var, new_arg):
        self.check_args(var, new_arg, mode="alpha")
        arg, body = self.nodes

        if var != arg and new_arg not in self.bound:
            body.alpha_convert(var, new_arg)

    def beta_reduce(self, var, new_term):
        self.check_args(var, new_term, mode="beta")
        arg, body = self.nodes

        if var != arg:
            if var not in body.bound and new_term in body.bound:
                new_term.alpha_convert(new_term, self.get_new_arg(var, new_term))

            body = body.beta_reduce(var, new_term)
            body.update_expr()
            self.nodes = [arg, body]

        return copy(body)

    def generate_bound(self):
        arg, body = self.nodes
        self.bound += deepcopy(body.bound) + [copy(arg)]

        self.propagate_bound()

    def update_expr(self):
        arg, body = self.nodes
        arg.update_expr()
        body.update_expr()

        self.expr = f"(λ{arg.expr}.{body.expr})"

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
            raise SyntaxError(f"'{expr}' has mismatched parentheses")

        # check 2: are there spaces or parentheses in expr?
        if " " not in expr and ("(" not in expr or ")" not in expr):
            return False

        # check 3: is expr an Builtin, Variable, or Abstraction?
        is_builtin = Builtin.check_grammar(expr)
        is_other_lambdaterm = Variable.check_grammar(expr) or Abstraction.check_grammar(expr)
        return not is_builtin and not is_other_lambdaterm

    def tokenize(self):
        start_right_child = None
        for idx in range(1, len(self.expr)):
            if self.expr.count("(", 0, idx) == self.expr.count(")", 0, idx):
                start_right_child = idx
                break

        left_child, right_child = self.expr[:start_right_child], self.expr[start_right_child:]
        self.nodes = [LambdaTerm.generate_tree(left_child), LambdaTerm.generate_tree(right_child)]

        if any(node.expr == self.expr for node in self.nodes):
            raise SyntaxError(f"'{self.expr}' is syntactically invalid")

    def alpha_convert(self, var, new_arg):
        self.check_args(var, new_arg, mode="alpha")
        left, right = self.nodes

        left.alpha_convert(var, new_arg)
        right.alpha_convert(var, new_arg)

    def beta_reduce(self, var, new_term):
        """Beta conversion is applied the same way as alpha conversion for Applications."""
        self.check_args(var, new_term, mode="beta")

        if var in self.bound:
            new_term.alpha_convert(new_term, self.get_new_arg(var, new_term))

        self.nodes = [node.beta_reduce(var, new_term) for node in self.nodes]
        self.update_expr()
        return self

    def generate_bound(self):
        left, right = self.nodes
        self.bound += deepcopy(left.bound) + deepcopy(right.bound)

        self.propagate_bound()

    def update_expr(self):
        left, right = self.nodes
        left.update_expr()
        right.update_expr()

        self.expr = f"({left.expr} {right.expr})"

    @property
    def tokenizable(self):
        return True

    @property
    def is_leftmost(self):
        """Applications are the only LambdaTerm that can be leftmost. An Application is leftmost if its left child is
        an Abstraction.
        """
        return isinstance(self.nodes[0], Abstraction)


class NormalOrderReducer:
    """Implements normal-order beta reduction of a syntax tree."""

    def __init__(self, expr):
        self.expr = expr
        self.tree = LambdaTerm.generate_tree(expr)

    def alpha_convert(self, var, new_arg):
        assert isinstance(self.tree, Abstraction), "top-level expr is not an Abstraction"

        arg, body = self.tree.nodes

        body.alpha_convert(var, new_arg)  # alpha-convert M
        arg.alpha_convert(var, new_arg)   # rename λvar.M -> λnew_arg.M

        self.tree.update_expr()
        self.expr = self.tree.expr

    def beta_reduce(self):
        reduced = None
        redex = self.tree.left_outer_redex()

        while redex is not None:
            abstraction, new_term = redex.nodes
            arg, body = abstraction.nodes

            reduced = body.beta_reduce(arg, new_term)
            reduced.update_expr()

            redex = reduced.left_outer_redex()

        return reduced

    def __repr__(self):
        return repr(self.tree)

    def __str__(self):
        return self.tree.display()


if __name__ == "__main__":
    syntax_tree = NormalOrderReducer("(λy.y x) x")
    print("TREE:", syntax_tree)
    print("\nFINAL:", syntax_tree.beta_reduce())
