"""Pure lambda calculus abstract syntax tree token generator and parser.

The `pure` directory contains pure lambda calculus AST generation and parsing- not sufficient for the lc language.

Formally, pure lambda calculus can be defined as

```
<λ-term> ::= <λ-term>                   ; "variable"
                                        ; - must be alphabetic character(s) (by convention, lowercase)
           | "λ" <λ-term> "." <λ-term>  ; "abstraction"
                                        ; - currying is not supported in this implementation (*)
                                        ; - abstraction bodies are greedy: λx.x y = λx.(x y) != (λx.x) (y)
           | <λ-term> <λ-term>          ; "application"
                                        ; - associating by left: abcd = ((((a) b) c) d)
```

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


class PureGrammar(ABC):
    """Superclass that represents any character/set of characters present in pure lambda calculus."""
    illegal = []
    SUBS = ["\u2080", "\u2081", "\u2082", "\u2083", "\u2084", "\u2085", "\u2086", "\u2087", "\u2088", "\u2089"]

    def __init__(self, expr):
        """Does not provide grammar check: call check_grammar manually before instantiating PureGrammar obj."""
        self.expr = PureGrammar.preprocess(expr)
        self._cls = type(self).__name__
        self.nodes = []

    @staticmethod
    @abstractmethod
    def check_grammar(expr, preprocess=True):
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

        for char in PureGrammar.illegal:
            if char in original_expr:
                raise SyntaxError(f"'{original_expr}' contains illegal character '{char}'")

        expr = original_expr.lstrip().rstrip()
        if expr[0] + expr[-1] == "()" and PureGrammar.are_parens_balanced(expr[1:-1]):
            expr = expr[1:-1]

        if original_expr == expr:
            return original_expr
        return PureGrammar.preprocess(expr)

    def display(self, indents=0):
        """Recursively displays PureGrammar tree with readable format.

        Format:
        <PureGrammar>(expr='<expr>', nodes=[
            <PureGrammar>(expr='<expr>', nodes=[
                ...
                <PureGrammar>(expr='<expr>')  # <-- if nodes is empty
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
        return isinstance(other, type(self)) and self.expr == other.expr


class Builtin(PureGrammar):
    """Built-in lambda calculus tokens: 'λ', '.', '(', ')' """
    TOKENS = ["λ", ".", "(", ")"]

    @staticmethod
    def check_grammar(expr, preprocess=True):
        if len(expr) == 1:
            return expr in Builtin.TOKENS
        if all(char in Builtin.TOKENS for char in expr):
            raise SyntaxError(f"'{expr}' has stray builtins")
        return False


class LambdaTerm(PureGrammar):
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
        self._flattened = {}

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
        """Given an abstraction λvar.M, this method renames all free occurences of var in M with new_arg. Proper usage
        of this method is provided in beta_reduce. Assumes var is a Variable and new_arg a LambdaTerm, and should not
        update self.expr with alpha-converted expr (unless self is Variable).
        """

    @abstractmethod
    def sub(self, var, new_term):
        """Given a redex (λvar.M)new_term, this method returns substitution of all free occurences of x with new_term.
        As with alpha_convert, this method is used in beta_reduce as a helper. Should call update_expr. Note that
        though this method does not perform a deep copy of self before returning, so the substituted and original nodes
        reference the same objects. However, beta_reduce in NormalOrderReducer should handle this properly and eliminate
        multiple references.
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

    @property
    def flattened(self):
        """Gets dict with keys being the Variables in self and the values being the paths to those Variables."""
        def get_flattened(node, path, flattened):
            if not node.tokenizable:
                if node in flattened:
                    flattened[node].append(path)
                else:
                    flattened[node] = [path]
            else:
                for idx, sub_node in enumerate(node.nodes):
                    get_flattened(sub_node, path + [idx], flattened)

        if not self._flattened:
            get_flattened(self, [], self._flattened)
        return self._flattened

    @classmethod
    def generate_tree(cls, expr, preprocess=True):
        """Converts expr to the proper LambdaTerm type, raises SyntaxError if expr is not a valid LambdaType."""
        for subclass_name in cls.__subclasses__():
            subclass = globals()[subclass_name.__name__]
            if subclass.check_grammar(expr, preprocess):
                return subclass(expr)
        raise SyntaxError(f"'{expr}' not valid LambdaTerm grammar")

    @classmethod
    def infer_type(cls, expr, preprocess=True):
        """Similar to generate_tree, but instead of generating a tree, just checks grammar of all subclasses."""
        for subclass_name in cls.__subclasses__():
            subclass = globals()[subclass_name.__name__]
            if subclass.check_grammar(expr, preprocess):
                return subclass

    def left_outer_redex(self):
        """Returns the index path to the leftmost outermost redex, if there is one. The first outermost node is returned
        without checking if it is leftmost, but generally first outermost node == leftmost outermost node.
        """

        def find_outer_redex(tree, path):
            if tree.is_leftmost:
                return path
            try:
                recursed = (find_outer_redex(node, path + [idx]) for idx, node in enumerate(tree.nodes))
                return next(filter(lambda node: node, recursed))
            except StopIteration:
                return None

        return find_outer_redex(self, [])

    def get_new_arg(self, var, new_term, body):
        """Returns the next term that isn't var nor arg and occurs in neither body nor new_term."""
        already_used = [var] + self.get_subnodes() + new_term.get_subnodes() + body.get_subnodes()

        subscript = 0
        while True:
            new_arg = Variable.subscript(new_term.expr, subscript)
            if new_arg not in already_used:
                return new_arg
            subscript += 1

    def get_subnodes(self):
        """Gets all subnodes of self."""

        def push_nodes(nodes, tree):
            if not tree.tokenizable:
                nodes.append(tree)
            else:
                for node in tree.nodes:
                    push_nodes(nodes, node)

        nodes_list = []
        push_nodes(nodes_list, self)
        return nodes_list

    def get(self, idxs):
        """Gets node at position specified by idxs. idxs=[] will return self."""
        if not idxs:
            return self

        this, *others = idxs
        if not others:
            assert this in (0, 1), f"idx must be 0 or 1, got {this}"
            return self.nodes[this]
        else:
            return self.nodes[this].get(others)

    def set(self, idxs, node):
        """Sets node at positions specified by idxs. idxs=[] will raise an error."""
        if not idxs:
            raise ValueError("idxs cannot be empty")

        if isinstance(node, Abstraction):
            node.expr = f"({node.expr})"

        this, *others = idxs
        if not others:
            assert this in (0, 1), f"idx must be 0 or 1, got {this}"
            self.nodes[this] = node
            self.update_expr()
        else:
            self.nodes[this].set(others, node)

    def __str__(self):
        return self.display()

    def __hash__(self):
        """Needed for _flattened dict, where Variables are keys."""
        return hash(self.expr)


class Variable(LambdaTerm):
    """Variable in lambda calculus: character(s) that represent abstractions."""
    INVALID = PureGrammar.illegal + Builtin.TOKENS + [" "]

    @staticmethod
    def check_grammar(expr, preprocess=True):
        if preprocess:
            expr = PureGrammar.preprocess(expr)
        return not any(char in Variable.INVALID for char in expr)

    def tokenize(self):
        """Variables are not tokenizable, so do nothing."""

    def alpha_convert(self, var, new_arg):
        if self == var:
            self.expr = new_arg.expr

    def sub(self, var, new_term):
        """Beta conversion is similar to alpha conversion for Variables, but types can change."""
        if self == var:
            return LambdaTerm.generate_tree(new_term.expr)  # type conversion might occur
        return Variable(self.expr)

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

    def like(self, other):
        """Checks whether self and other are equal, ignoring subscripts."""

        def strip_subscript(expr):
            while expr[-1] in PureGrammar.SUBS:
                expr = expr[:-1]
            return expr

        return self == other or strip_subscript(self.expr) == strip_subscript(other.expr)

    @classmethod
    def subscript(cls, var, num):
        """Returns var with subscript of n."""
        while var[-1] in PureGrammar.SUBS:
            var = var[:-1]  # remove any subscripts

        subscripted = var
        for digit in PureGrammar.preprocess(str(num)):
            subscripted += PureGrammar.SUBS[int(digit)]
        return cls(subscripted)


class Abstraction(LambdaTerm):
    """Abstraction: the basic datatype in lambda calculus."""

    @staticmethod
    def check_grammar(expr, preprocess=True):
        """This implementation of Abstraction differs from the actual lambda calculus definition.
        - Actual Abstraction format: "λ" <Variable> "." <LambdaTerm> (optional parentheses not shown)
        - Accepted Abstraction format: "λ" <Variable> "." <anything> (optional parentheses not shown)

        The reason for this discrepancy is to delegate recursive token parsing to LambdaAST.
        """
        if preprocess:
            expr = PureGrammar.preprocess(expr)

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
        arg = expr[expr.index("λ") + 1:expr.index(".")]
        if not Variable.check_grammar(arg, False):
            return False
        elif any(Builtin.check_grammar(char) for char in arg):
            raise SyntaxError(f"'{expr}' contains an illegal bound variable")

        # check 3: is body valid?
        body = expr[expr.index(".") + 1:]
        if len(body) == 0 or all(Builtin.check_grammar(char) for char in body):
            raise SyntaxError(f"'{expr}' contains an illegal abstraction body")
        return True

    def tokenize(self):
        arg = Variable(self.expr[self.expr.index("λ") + 1:self.expr.index(".")])
        body = LambdaTerm.generate_tree(self.expr[self.expr.index(".") + 1:])

        self.nodes = [arg, body]

    def alpha_convert(self, var, new_arg):
        if new_arg in self.get_subnodes():
            raise ValueError(f"'{new_arg.expr}' already in '{self.expr}'")

        arg, body = self.nodes
        if var != arg:
            body.alpha_convert(var, new_arg)

    def sub(self, var, new_term):
        arg, body = self.nodes

        if var != arg:
            if arg not in new_term.bound and arg in new_term.get_subnodes():  # if arg is free in new_term
                new_arg = new_term.get_new_arg(var, arg, body)

                body.alpha_convert(arg, new_arg)  # must convert body first because arg changes
                arg.alpha_convert(arg, new_arg)

            self.nodes = [arg, body.sub(var, new_term)]

        return self

    def generate_bound(self):
        arg, body = self.nodes
        self.bound = body.bound + [arg]

    def update_expr(self):
        arg, body = self.nodes
        arg.update_expr()
        body.update_expr()

        self.expr = f"λ{arg.expr}."
        if isinstance(body, Application):
            self.expr += f"({body.expr})"
        else:
            self.expr += f"{body.expr}"

    @property
    def tokenizable(self):
        return True

    @property
    def is_leftmost(self):
        return False


class Application(LambdaTerm):
    """Application of arbitrary number of Abstractions/Variables."""

    @staticmethod
    def check_grammar(expr, preprocess=True):
        """This implementation of Application differs from the actual lambda calculus definition.
        - Actual Application format: <LambdaTerm>+ (grouped by parentheses and spaces)
        - Accepted Application format: anything but other LambdaTerms, grouped correctly by parens/spaces
        """
        if preprocess:
            expr = PureGrammar.preprocess(expr)

        # check 1: are parentheses balanced?
        if not PureGrammar.are_parens_balanced(expr):
            raise SyntaxError(f"'{expr}' has mismatched parentheses")

        # check 2: are there spaces or parentheses in expr?
        if " " not in expr and ("(" not in expr or ")" not in expr):
            return False

        # check 3: is expr an Builtin, Variable, or Abstraction?
        is_builtin = Builtin.check_grammar(expr)
        is_other_lambdaterm = Variable.check_grammar(expr, False) or Abstraction.check_grammar(expr, False)
        return not is_builtin and not is_other_lambdaterm

    def tokenize(self):
        bind_pos = -1
        for idx, char in enumerate(self.expr):
            if char == "λ" and self.expr.count("(", 0, idx + 1) == self.expr.count(")", 0, idx + 1):
                bind_pos = idx
                break

        start_right_child = None
        for idx in range(len(self.expr) - 1, 0, -1):
            to_iterate = self.expr[:idx]

            paren_balance = to_iterate.count("(") == to_iterate.count(")")
            past_bind = bind_pos != -1 and idx > bind_pos
            multichar_var = Variable.check_grammar(self.expr[idx - 1:])

            initial_tests = paren_balance and not past_bind and not multichar_var

            if initial_tests and LambdaTerm.infer_type(to_iterate, False) is not None:
                start_right_child = idx
                break

        left_child, right_child = self.expr[:start_right_child], self.expr[start_right_child:]
        self.nodes = [LambdaTerm.generate_tree(left_child), LambdaTerm.generate_tree(right_child)]

    def alpha_convert(self, var, new_arg):
        if new_arg in self.get_subnodes():
            raise ValueError(f"'{new_arg.expr}' is present in '{self.expr}'")

        left, right = self.nodes
        left.alpha_convert(var, new_arg)
        right.alpha_convert(var, new_arg)

    def sub(self, var, new_term):
        """Beta conversion is applied like alpha conversion for Applications."""
        self.nodes = [node.sub(var, new_term) for node in self.nodes]

        return self

    def generate_bound(self):
        left, right = self.nodes
        self.bound = left.bound + right.bound

    def update_expr(self):
        left, right = self.nodes

        left.update_expr()
        right.update_expr()

        self.expr = f""
        for node in self.nodes:
            if not node.tokenizable:
                self.expr += f"{node.expr} "
            else:
                self.expr += f"({node.expr}) "
        self.expr = self.expr.rstrip()

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
    """Implements normal-order beta reduction of a syntax tree. Used instead of LambdaTerm in lang."""

    def __init__(self, expr, reduce=False):
        self._expr = expr
        self.tree = LambdaTerm.generate_tree(self._expr)

        self.reduced = False
        if reduce:
            self.beta_reduce()

    def beta_reduce(self):
        """In-place normal-order beta reduction of self.tree."""
        redex_path = self.tree.left_outer_redex()

        while redex_path is not None:
            redex = self.tree.get(redex_path)

            abstraction, new_term = redex.nodes
            arg, body = abstraction.nodes

            self.set(redex_path, body.sub(arg, new_term))

            redex_path = self.tree.left_outer_redex()

        self.tree.expr = PureGrammar.preprocess(self.tree.expr)  # for greater readability
        self.reduced = True

    def set(self, idxs, node):
        """Sets self.tree with node at position specified by idxs. An empty list will replace self.tree with node."""
        try:
            self.tree.set(idxs, node)
        except ValueError:
            self.tree = node
        self.tree.update_expr()

    @property
    def flattened(self):
        return self.tree.flattened

    def __repr__(self):
        return repr(self.tree)

    def __str__(self):
        return self.tree.display()
