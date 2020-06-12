"""Pure lambda calculus abstract syntax tree token generator and parser.

The `pure` directory contains pure lambda calculus AST generation and parsing- not sufficient for the lcalc language.

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

(*) Why is currying not supported? Because it makes the use of multi-character Variables ambiguous. For example, if
currying is allowed, what does the expression `λvar.x` mean? Should it be resolved to `λv.λa.λr.x`, or is `var` a
Variable name? Thus, currying and multi-character Variable cannot coexist without causing ambiguity. This
implementation favors multi-character Variables over currying, a purely arbitrary decision. Relatedly, this feature
means that function application must be separated by spaces or parentheses.
"""

from abc import abstractmethod, ABC
from copy import deepcopy
from itertools import zip_longest

from lang.error import GenericException


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
        """This method should check expr's top-level grammar and return whether or not it is valid. It should also
        raise a SyntaxError if expr's top-level grammar is similar to the accepted grammar but syntactically invalid.
        """

    @property
    @abstractmethod
    def tokenizable(self):
        """Whether or not this object is tokenizable."""

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
            raise GenericException("λ-term cannot be empty", original_expr)

        for char in PureGrammar.illegal:
            if char in pre_expr:
                pos = pre_expr.index(char) + original_expr.index(pre_expr)
                msg = "'{}' contains reserved character '{}'"
                raise GenericException(msg, (original_expr, char), start=pos, end=pos + len(char))

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
            for idx, char in enumerate(original_expr):
                if char in Builtin.TOKENS:
                    raise GenericException("'{}' has stray builtin '{}'", (original_expr, char), start=idx, end=idx + 1)
        return False

    @property
    def tokenizable(self):
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

    @abstractmethod
    def tokenize(self):
        """This method should recursively tokenize a LambdaTerm. Assumes top-level grammar has been checked, but raises
        an error if second-level grammar is not valid. Should set self.nodes attribute, and will always generate two
        nodes (lambda calculus ASTs are binary), even for Applications.
        """

    @abstractmethod
    def alpha_convert(self, used, bound, current_depth=0):
        """Given used dictionary (arg: whether or not in arg's abstraction body) and bound dictionary
        (arg: [new args, reverse precedence]), this method should rename all variables that would become ambiguous
        during beta reduction. Should be run after generate_tree and before sub. Alternative to alpha reduction during
        sub.
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
    def update_expr(self):
        """Updates self.expr from self.nodes. Used during alpha_convert."""

    @abstractmethod
    def alpha_equals(self, other, mapping=None):
        """Whether or not two LambdaTerms are alpha-equivalent. mapping represents map between self vars and other vars
        that are alpha-equivalent to their corresponding key entry. other_mapping is similar to mapping but from
        perspective of other.
        """

    @property
    @abstractmethod
    def is_leftmost(self):
        """Whether or not this object is a leftmost node in a syntax tree. Only works if tokenize has been run."""

    def flattened(self, recompute=False):
        """Gets dict with keys being the sub node exprs in self and the values being a tuple of (node, paths to those
        sub nodes).
        """
        dummy = (None, [])

        def generate_flattened(node, path, flattened):
            for idx, sub_node in enumerate(node.nodes):
                flattened[sub_node.expr] = (sub_node, flattened.get(sub_node.expr, dummy)[-1] + [path + [idx]])
                generate_flattened(sub_node, path + [idx], flattened)

        if not self._flattened or recompute:
            self._flattened = {}
            generate_flattened(self, [], self._flattened)
        return self._flattened

    @classmethod
    def generate_tree(cls, expr, original_expr=None, preprocess=True):
        """Converts expr to the proper LambdaTerm type, raises SyntaxError if expr is not a valid LambdaType."""
        for subclass_name in cls.__subclasses__():
            subclass = globals()[subclass_name.__name__]
            if subclass.check_grammar(expr, original_expr, preprocess):
                return subclass(expr, original_expr)
        raise GenericException("'{}' is not valid λ-term grammar", original_expr)

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

    def get(self, idxs):
        """Gets node at positions specified by idxs. idxs=[] will return self."""
        if not idxs:
            return self

        this, *others = idxs
        if not others:
            return self.nodes[this]
        return self.nodes[this].get(others)

    def set(self, idxs, node):
        """Sets node at positions specified by idxs. idxs=[] will raise an error."""
        if not idxs:
            raise ValueError("idxs cannot be empty")

        this, *others = idxs
        if not others:
            self.nodes[this] = node
            self.update_expr()
        else:
            self.nodes[this].set(others, node)
            self.update_expr()

    def __str__(self):
        return self.display()


class Variable(LambdaTerm):
    """Variable in lambda calculus: character(s) that represent abstractions."""

    @staticmethod
    def check_grammar(expr, original_expr, preprocess=True):
        if preprocess:
            expr = PureGrammar.preprocess(expr, original_expr)
        if not any(char in Builtin.TOKENS + [" "] for char in expr):
            return not any(phrase in expr for phrase in PureGrammar.illegal)
        return False

    def tokenize(self):
        """Variables are not tokenizable, so do nothing."""

    def alpha_convert(self, used, bound, current_depth=0):
        if self.expr in bound:
            self.expr = bound[self.expr][-1]
        elif used.get(self.expr, current_depth) != current_depth:
            self.expr = self.get_new_arg(used, current_depth).expr

        if self.expr not in used:
            used[self.expr] = current_depth

        self.update_expr()

    def sub(self, var, new_term):
        if self == var:
            return deepcopy(new_term)  # deepcopy to avoid infinite recursion
        return self

    def generate_bound(self):
        """Variables have no nodes and therefore no bound variables."""

    def update_expr(self):
        """Variables have no nodes, so do nothing."""

    def alpha_equals(self, other, mapping=None, other_mapping=None):
        if mapping is None:
            mapping = {}
        if other_mapping is None:
            other_mapping = {}

        if not isinstance(other, type(self)):
            return False

        if self.expr in mapping:
            return mapping[self.expr][-1] == other.expr
        elif other.expr in other_mapping:
            return other_mapping[other.expr][-1] == self.expr

        mapping[self.expr] = [other.expr]
        other_mapping[other.expr] = [self.expr]

        return True

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
        return cls(subscripted)

    @staticmethod
    def split(expr):
        """Splits expr into var and subscript."""
        subscript = []
        while expr[-1] in PureGrammar.SUBS:
            subscript.insert(0, PureGrammar.SUBS.index(expr[-1]))
            expr = expr[:-1]
        return expr, int("".join(str(sub) for sub in subscript)) if subscript else -1

    def get_new_arg(self, used, current_depth):
        """Returns the next term that is like arg but isn't in used."""
        arg, __ = Variable.split(self.expr)
        max_subscript = -1

        for expr in used:
            var, subscript = Variable.split(expr)
            if var == arg and subscript > max_subscript:
                max_subscript = subscript

        new_arg = Variable.subscript(arg, max_subscript + 1)

        if new_arg.expr not in used:
            used[new_arg.expr] = current_depth

        return new_arg


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
            start = max(original_expr.rfind("λ"), original_expr.rfind("."))
            raise GenericException("'{}' has mismatched binds/declarators", original_expr, start=start, end=start + 1)

        elif decl < bind:
            decl += original_expr.index(expr)
            raise GenericException("'{}' has declarator before bind", original_expr, start=decl, end=decl + 1)

        elif bind != 0:
            return False

        # check 2: is bound variable valid?
        arg = expr[expr.index("λ") + 1:expr.index(".")]
        if not Variable.check_grammar(arg, original_expr, False):
            return False

        # check 3: is body valid?
        body = expr[expr.index(".") + 1:]
        if len(body) == 0 or all(Builtin.check_grammar(char, original_expr) for char in body):
            start = expr.index(body) + original_expr.index(expr)
            raise GenericException("'{}' contains an illegal abstraction body", original_expr, start=start)

        return True

    def tokenize(self):
        arg = Variable(self.expr[self.expr.index("λ") + 1:self.expr.index(".")], self.original_expr)
        body = LambdaTerm.generate_tree(self.expr[self.expr.index(".") + 1:], self.original_expr)

        self.nodes = [arg, body]

    def alpha_convert(self, used, bound, current_depth=0):
        arg, body = self.nodes

        to_remove = arg.expr
        if arg.expr in used:
            new_arg = arg.get_new_arg(used, current_depth)
        else:
            new_arg = arg

        bound[arg.expr] = bound.get(arg.expr, []) + [new_arg.expr]
        arg.expr = new_arg.expr

        body.alpha_convert(used, bound, current_depth)
        self.nodes = [arg, body]

        bound[to_remove].pop()
        if not bound[to_remove]:
            del bound[to_remove]

        self.update_expr()

    def sub(self, var, new_term):
        arg, body = self.nodes
        if var != arg:
            self.nodes = [arg, body.sub(var, new_term)]
            self.update_expr()
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

    def alpha_equals(self, other, mapping=None, other_mapping=None):
        if mapping is None:
            mapping = {}
        if other_mapping is None:
            other_mapping = {}

        if not isinstance(other, type(self)):
            return False

        arg, body = self.nodes
        other_arg, other_body = other.nodes

        mapping[arg.expr] = mapping.get(arg.expr, []) + [other_arg.expr]
        other_mapping[other_arg.expr] = other_mapping.get(other_arg.expr, []) + [arg.expr]

        return body.alpha_equals(other_body, mapping, other_mapping)

    @property
    def tokenizable(self):
        return True

    @property
    def is_leftmost(self):
        return False


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
            raise GenericException("'{}' has mismatched parentheses", original_expr)

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
            raise GenericException("'{}' is invalid λ-term grammar", self.original_expr)

        self.nodes = [
            LambdaTerm.generate_tree(self.expr[:start_right_child], self.original_expr),
            LambdaTerm.generate_tree(self.expr[start_right_child:], self.original_expr)
        ]

    def alpha_convert(self, used, bound, current_depth=0):
        left, right = self.nodes
        left.alpha_convert(used, bound, current_depth + 1)
        right.alpha_convert(used, bound, current_depth + 1)

        self.update_expr()

    def sub(self, var, new_term):
        self.nodes = [node.sub(var, new_term) for node in self.nodes]
        self.update_expr()
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

    def alpha_equals(self, other, mapping=None, other_mapping=None):
        if mapping is None:
            mapping = {}
        if other_mapping is None:
            other_mapping = {}

        if not isinstance(other, type(self)):
            return False

        for node, other_node in zip(self.nodes, other.nodes):
            if not node.alpha_equals(other_node, mapping, other_mapping):
                return False
        return True

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
    RECURSION_LIMIT = 100

    def __init__(self, expr, original_expr=None):
        self.original_expr = original_expr if original_expr else expr
        self.tree = LambdaTerm.generate_tree(expr, self.original_expr)

        self.used = {}
        self.bound = {}

        self.reduced = False
        self._flattened = {}

    @staticmethod
    def _has_pattern(diffs):
        """Whether or not diffs exhibits a repeated pattern. Used to detect non-beta-reducable exprs."""
        def grouper(iterable, n, fillvalue=None):
            # https://stackoverflow.com/questions/434287/what-is-the-most-pythonic-way-to-iterate-over-a-list-in-chunks
            args = [iter(iterable)] * n
            return zip_longest(*args, fillvalue=fillvalue)

        pattern = []
        for idx, diff in enumerate(diffs):
            if diff not in pattern:
                pattern.append(diff)
            else:
                start = idx
                break
        else:
            return False

        for group in grouper(diffs[start:], len(pattern), 0):
            if list(group) != pattern:
                return False
        return True

    def beta_reduce(self, error_handler):
        """In-place normal-order beta reduction of self.tree. error_handler is the current session's error handler."""
        self.tree.alpha_convert(self.used, self.bound)
        error_handler.register_step("α", self.tree.expr)

        diffs = []
        redex, redex_path = self.tree.left_outer_redex()

        while redex_path is not None:
            abstraction, new_term = redex.nodes
            arg, body = abstraction.nodes

            prev_len = len(self.tree.expr)
            self.set(redex_path, body.sub(arg, new_term))
            error_handler.register_step("β", self.tree.expr)

            diffs.append(len(self.tree.expr) - prev_len)
            redex, redex_path = self.tree.left_outer_redex()

            if len(diffs[NormalOrderReducer.RECURSION_LIMIT:]) > NormalOrderReducer.RECURSION_LIMIT:
                to_check = diffs[len(diffs) - NormalOrderReducer.RECURSION_LIMIT:]  # get last 100 elems
                if NormalOrderReducer._has_pattern(to_check):
                    error_handler.warn("'{}' does not have a beta-normal form", self.original_expr)
                    break

        self.tree.expr = PureGrammar.preprocess(self.tree.expr)
        self.reduced = True

        self.used = {}
        self.bound = {}

    def get(self, idxs):
        """Gets node specified by idxs."""
        return self.tree.get(idxs)

    def set(self, idxs, node):
        """Sets self.tree with node at position specified by idxs. An empty list will replace self.tree with node.
        """
        try:
            self.tree.set(idxs, node)
        except ValueError:
            self.tree = node
            self.tree.update_expr()

    def flattened(self, recompute=False):
        """Returns self.tree.flattened, plus top-level expr."""
        if not self._flattened or recompute:
            self._flattened = self.tree.flattened(recompute)
            self._flattened[self.tree.expr] = (self.tree, [[]])
        return self._flattened

    def __repr__(self):
        return repr(self.tree)

    def __str__(self):
        return self.tree.display()
