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
from copy import deepcopy

# from lang.error import template
def template(msg, *args, **kwargs):
    try:
        exc = args[0]
        if isinstance(exc, str):
            exc = [exc]
        return msg.format(*exc)
    except (TypeError, IndexError):
        return msg

class PureGrammar(ABC):
    """Superclass that represents any character/set of characters present in pure lambda calculus."""
    illegal = []
    SUBS = ["\u2080", "\u2081", "\u2082", "\u2083", "\u2084", "\u2085", "\u2086", "\u2087", "\u2088", "\u2089"]

    def __init__(self, expr, original_expr):
        """Does not provide grammar check: call check_grammar manually before instantiating PureGrammar obj.
        original_expr is used for better error messages.
        """
        self.original_expr = original_expr.strip()
        self.expr = PureGrammar.preprocess(expr, original_expr)
        self._cls = type(self).__name__
        self.nodes = []

    @staticmethod
    @abstractmethod
    def check_grammar(expr, original_expr, preprocess=True):
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
    def preprocess(pre_expr, original_expr=None):
        """Strips surrounding whitespace and outer parentheses (if any) from expr."""
        if not original_expr:
            original_expr = pre_expr

        if not pre_expr:
            raise ValueError(template("Lambda term cannot be empty", internal=True))

        for char in PureGrammar.illegal:
            if char in pre_expr:
                pos = pre_expr.index(char)
                msg = "'{}' contains reserved character '{}'"
                raise SyntaxError(template(msg, (original_expr, char), start=pos, end=pos + 1))

        expr = pre_expr.strip()
        if expr[0] + expr[-1] == "()" and PureGrammar.are_parens_balanced(expr[1:-1]):
            expr = expr[1:-1]

        if pre_expr == expr:
            return pre_expr
        return PureGrammar.preprocess(expr, original_expr)

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
    def check_grammar(expr, original_expr, preprocess=True):
        if len(expr) == 1:
            return expr in Builtin.TOKENS
        if all(char in Builtin.TOKENS for char in expr):
            for idx, char in enumerate(original_expr):  # for loop is just used for better error msg
                if char in Builtin.TOKENS:
                    msg = "'{}' has stray builtin '{}'"
                    raise SyntaxError(template(msg, (original_expr, char), start=idx, end=idx + 1))
        return False


class LambdaTerm(PureGrammar):
    """Represents a valid λ-term: variable, abstraction, or application. Also abstractly defines functionality that
    will allow a syntax tree to be built.
    """

    def __init__(self, expr, original_expr=None):
        """In-place recursive generation of syntax tree from a single LambdaTerm object."""
        super().__init__(expr, original_expr if original_expr else expr)
        self.bound = []
        self._flattened = {}

        self.tokenize()
        self.update_expr()
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
    def sub(self, var, new_term, subscript_cache):
        """Given a redex (λvar.M)new_term, this method returns substitution of all free occurences of x with new_term.
        As with alpha_convert, this method is used in beta_reduce as a helper. Should call update_expr. Note that
        though this method does not perform a deep copy of self before returning, so the substituted and original nodes
        reference the same objects. However, beta_reduce in NormalOrderReducer should handle this properly and eliminate
        multiple references. subscript cache is used for keeping track of used subscripts.
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
    def generate_tree(cls, expr, original_expr=None, preprocess=True):
        """Converts expr to the proper LambdaTerm type, raises SyntaxError if expr is not a valid LambdaType.
        original_expr is used for displaying error messages.
        """
        for subclass_name in cls.__subclasses__():
            subclass = globals()[subclass_name.__name__]
            if subclass.check_grammar(expr, original_expr, preprocess):
                return subclass(expr, original_expr)
        raise SyntaxError(template("'{}' is not valid λ-term grammar", original_expr))

    @classmethod
    def infer_type(cls, expr, original_expr=None, preprocess=True):
        """Similar to generate_tree, but instead of generating a tree, just checks grammar of all subclasses."""
        for subclass_name in cls.__subclasses__():
            subclass = globals()[subclass_name.__name__]
            if subclass.check_grammar(expr, original_expr, preprocess):
                return subclass

    def left_outer_redex(self):
        """Returns the index path to the leftmost outermost redex, if there is one. The first outermost node is returned
        without checking if it is leftmost, but generally first outermost node == leftmost outermost node.
        """

        def find_outer_redex(tree, path):
            if tree.is_leftmost:
                return tree, path
            for idx, node in enumerate(tree.nodes):
                result = find_outer_redex(node, path + [idx])
                if result:
                    return result

        try:
            redex, redex_path = find_outer_redex(self, [])
            return redex, redex_path
        except TypeError:
            return None, None

    def set(self, idxs, node):
        """Sets node at positions specified by idxs. idxs=[] will raise an error."""
        if not idxs:
            raise ValueError(template("idxs cannot be empty", internal=True))

        this, *others = idxs
        if not others:
            self.nodes[this] = node
        else:
            self.nodes[this].set(others, node)

    def __str__(self):
        return self.display()

    def __hash__(self):
        """Needed for _flattened dict, where Variables are keys."""
        return hash(self.expr)


class Variable(LambdaTerm):
    """Variable in lambda calculus: character(s) that represent abstractions."""
    INVALID = Builtin.TOKENS + [" "]

    @staticmethod
    def check_grammar(expr, original_expr, preprocess=True):
        if preprocess:
            expr = PureGrammar.preprocess(expr, original_expr)
        if not any(char in Builtin.TOKENS + [" "] for char in expr):
            return not any(char in expr for char in PureGrammar.illegal)
        return False

    def tokenize(self):
        """Variables are not tokenizable, so do nothing."""

    def alpha_convert(self, var, new_arg):
        if self == var:
            self.expr = new_arg.expr
            self.original_expr = new_arg.original_expr

    def sub(self, var, new_term, subscript_cache):
        """Beta conversion is similar to alpha conversion for Variables, but types can change."""
        if self == var:
            return deepcopy(new_term)  # deepcopy to avoid infinite recursion
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

    @classmethod
    def subscript(cls, var, num):
        """Returns var with subscript of n."""
        subscripted = var + "".join(PureGrammar.SUBS[int(digit)] for digit in PureGrammar.preprocess(str(num)))
        return cls(subscripted, subscripted)

    @staticmethod
    def strip_subscripts(expr):
        """Strips subscripts from expr."""
        while expr[-1] in PureGrammar.SUBS:
            expr = expr[:-1]  # remove subscripts
        return expr


class Abstraction(LambdaTerm):
    """Abstraction: the basic datatype in lambda calculus."""

    @staticmethod
    def check_grammar(expr, original_expr, preprocess=True):
        """This implementation of Abstraction differs from the actual lambda calculus definition.
        - Actual Abstraction format: "λ" <Variable> "." <LambdaTerm> (optional parentheses not shown)
        - Accepted Abstraction format: "λ" <Variable> "." <anything> (optional parentheses not shown)

        The reason for this discrepancy is to delegate recursive token parsing to LambdaAST.
        """
        if preprocess:
            expr = PureGrammar.preprocess(expr, original_expr)

        # check 1: are required Builtins (λ, .) correctly placed and matched within expr?
        bind = expr.find("λ")
        decl = expr.find(".")

        if bind == decl:
            return False

        elif expr.count("λ") != expr.count(".") or (bind != -1 and decl == -1) or (decl != -1 and bind == -1):
            msg = "'{}' has mismatched binds/declarators"
            start = max(original_expr.find("λ"), original_expr.find("."))
            raise SyntaxError(template(msg, original_expr, start=start, end=start + 1))

        elif decl < bind:
            msg = "'{}' has declarator before bind"
            decl = original_expr.find(".")
            raise SyntaxError(template(msg, original_expr, start=decl, end=decl + 1))

        elif bind != 0:
            return False

        # check 2: is bound variable valid?
        arg = expr[expr.index("λ") + 1:expr.index(".")]
        if not Variable.check_grammar(arg, original_expr, False):
            return False

        elif any(Builtin.check_grammar(char, original_expr) for char in arg):
            start = original_expr.index("λ") + 1
            end = original_expr.index(".")
            raise SyntaxError(template("'{}' contains an illegal bound variable", original_expr, start=start, end=end))

        # check 3: is body valid?
        body = expr[expr.index(".") + 1:]
        if len(body) == 0 or all(Builtin.check_grammar(char, original_expr) for char in body):
            start = original_expr[original_expr.index(".") + 1:]
            raise SyntaxError(template("'{}' contains an illegal abstraction body", original_expr, start=start))

        return True

    def tokenize(self):
        arg = Variable(self.expr[self.expr.index("λ") + 1:self.expr.index(".")], self.original_expr)
        body = LambdaTerm.generate_tree(self.expr[self.expr.index(".") + 1:], self.original_expr)

        self.nodes = [arg, body]

    def alpha_convert(self, var, new_arg):
        arg, body = self.nodes
        if var != arg:
            body.alpha_convert(var, new_arg)

    def sub(self, var, new_term, subscript_cache):
        print(f"Abstraction.sub: \n    expr: {self.expr}\n    var: {var}\n    new_term: {new_term}")
        arg, body = self.nodes
        if var != arg:
            if arg not in new_term.bound:
                print(f"    old arg: {arg}", end="")
                new_arg = self.get_new_arg(subscript_cache)
                print(f" new arg: {new_arg}, new_term.bound: {new_term.bound}")

                body.alpha_convert(arg, new_arg)  # must convert body first because arg changes
                arg.alpha_convert(arg, new_arg)

            self.nodes = [arg, body.sub(var, new_term, subscript_cache)]

        return self

    def generate_bound(self):
        arg, body = self.nodes
        self.bound = body.bound + [arg]

    def update_expr(self):
        arg, body = self.nodes

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

    def get_new_arg(self, subscript_cache):
        """Returns the next term that is like arg but isn't in subscript cache."""
        arg, body = self.nodes
        arg.expr = Variable.strip_subscripts(arg.expr)

        already_used = "".join(subscript_cache)
        max_subscript = -1

        for idx in range(len(already_used) - 1):
            char, next_char = already_used[idx], already_used[idx + 1]

            if char == arg.expr and next_char in PureGrammar.SUBS:
                subscript = PureGrammar.SUBS.index(next_char)

                if subscript > max_subscript:
                    max_subscript = subscript

        subscripted = Variable.subscript(arg.expr, max_subscript + 1)
        subscript_cache.append(subscripted.expr)

        return subscripted


class Application(LambdaTerm):
    """Application of arbitrary number of Abstractions/Variables."""

    @staticmethod
    def check_grammar(expr, original_expr, preprocess=True):
        """This implementation of Application differs from the actual lambda calculus definition.
        - Actual Application format: <LambdaTerm>+ (grouped by parentheses and spaces)
        - Accepted Application format: anything but other LambdaTerms, grouped correctly by parens/spaces
        """
        if preprocess:
            expr = PureGrammar.preprocess(expr, original_expr)

        # check 1: are parentheses balanced? (only check if preprocess)
        if preprocess and not PureGrammar.are_parens_balanced(expr):
            raise SyntaxError(template("'{}' has mismatched parentheses", original_expr))

        # check 2: are there spaces or parentheses in expr?
        if " " not in expr and ("(" not in expr or ")" not in expr):
            return False

        # check 3: is expr an Builtin, Variable, or Abstraction?
        if Builtin.check_grammar(expr, original_expr, False):
            return False
        elif Variable.check_grammar(expr, original_expr, False):
            return False
        elif Abstraction.check_grammar(expr, original_expr, False):
            return False
        return True

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
            multichar_var = Variable.check_grammar(self.expr[idx - 1:], self.original_expr)

            initial_tests = paren_balance and not past_bind and not multichar_var

            if initial_tests and LambdaTerm.infer_type(to_iterate, self.original_expr, False) is not None:
                start_right_child = idx
                break

        if start_right_child is None:
            raise SyntaxError(template("'{}' is invalid LambdaTerm grammar", self.original_expr))

        self.nodes = [
            LambdaTerm.generate_tree(self.expr[:start_right_child], self.original_expr),
            LambdaTerm.generate_tree(self.expr[start_right_child:], self.original_expr)
        ]

    def alpha_convert(self, var, new_arg):
        left, right = self.nodes
        left.alpha_convert(var, new_arg)
        right.alpha_convert(var, new_arg)

    def sub(self, var, new_term, subscript_cache):
        """Beta conversion is applied like alpha conversion for Applications."""
        self.nodes = [node.sub(var, new_term, subscript_cache) for node in self.nodes]
        return self

    def generate_bound(self):
        left, right = self.nodes
        self.bound = left.bound + right.bound

    def update_expr(self):
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
        self.tree = LambdaTerm.generate_tree(expr)
        print(self.tree.expr)
        self.subscript_cache = []

        self.reduced = False
        if reduce:
            self.beta_reduce()

    def beta_reduce(self):
        """In-place normal-order beta reduction of self.tree."""
        redex, redex_path = self.tree.left_outer_redex()

        while redex_path is not None:
            abstraction, new_term = redex.nodes
            arg, body = abstraction.nodes

            self.set(redex_path, body.sub(arg, new_term, self.subscript_cache))
            redex, redex_path = self.tree.left_outer_redex()

        self.tree.update_expr()
        self.tree.expr = PureGrammar.preprocess(self.tree.expr)  # for greater readability

        self.reduced = True

    def set(self, idxs, node):
        """Sets self.tree with node at position specified by idxs. An empty list will replace self.tree with node."""
        try:
            self.tree.set(idxs, node)
        except ValueError:
            self.tree = node

    @property
    def flattened(self):
        return self.tree.flattened

    def __repr__(self):
        return repr(self.tree)

    def __str__(self):
        return self.tree.display()


if __name__ == "__main__":
    import time

    s = time.time()
    print(NormalOrderReducer("(λm.λn.((n (λn.λf.λx.(f ((n f) x)))) m)) (λf.λx.(f (f (f x))))", reduce=True))
    print(time.time() - s)
