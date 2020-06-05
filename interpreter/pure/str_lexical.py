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

# from lang.error import template
def template(msg, *args, **kwargs):
    try:
        exc = args[0]
        if isinstance(exc, str):
            exc = [exc]
        return msg.format(*exc)
    except TypeError:
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

    def __init__(self, expr, original_expr=None, generate=True):
        super().__init__(expr, original_expr if original_expr else expr)

        if generate:
            self.tokenize()

    @abstractmethod
    def tokenize(self):
        """This method should recursively tokenize a LambdaTerm. Assumes top-level grammar has been checked, but raises
        an error if second-level grammar is not valid. Should set self.get_children() attribute, and will always generate two
        nodes (lambda calculus ASTs are binary), even for Applications.
        """

    @staticmethod
    @abstractmethod
    def get_expr(left, right, original_expr):
        ...
    
    @abstractmethod
    def _get_children(self):
        ...

    @abstractmethod
    def _alpha_convert(self, var, new_arg):
        """Given an abstraction λvar.M, this method renames all free occurences of var in M with new_arg. Proper usage
        of this method is provided in beta_reduce. Assumes var is a Variable and new_arg a LambdaTerm, and should not
        update self.expr with alpha-converted expr (unless self is Variable).
        """

    @abstractmethod
    def _sub(self, var, new_term, subscript_cache):
        """Given a redex (λvar.M)new_term, this method returns substitution of all free occurences of x with new_term.
        As with alpha_convert, this method is used in beta_reduce as a helper. Should call update_expr. Note that
        though this method does not perform a deep copy of self before returning, so the substituted and original nodes
        reference the same objects. However, beta_reduce in NormalOrderReducer should handle this properly and eliminate
        multiple references.
        """

    @staticmethod
    def replace(expr, old, new):
        start = expr.index(old)
        return expr[:start] + new + expr[start + len(old):]

    @classmethod
    def generate_tree(cls, expr, original_expr=None, preprocess=False, generate=False):
        """Converts expr to the proper LambdaTerm type, raises SyntaxError if expr is not a valid LambdaType.
        original_expr is used for displaying error messages.
        """
        for subclass_name in cls.__subclasses__():
            subclass = globals()[subclass_name.__name__]
            if subclass.check_grammar(expr, original_expr, preprocess):
                return subclass(expr, original_expr, generate)
        raise SyntaxError(template("'{}' is not valid λ-term grammar", original_expr))

    @classmethod
    def infer_type(cls, expr, original_expr=None, preprocess=True):
        """Similar to generate_tree, but instead of generating a tree, just checks grammar of all subclasses."""
        for subclass_name in cls.__subclasses__():
            subclass = globals()[subclass_name.__name__]
            if subclass.check_grammar(expr, original_expr, preprocess):
                return subclass

    def is_leftmost(self):
        if not isinstance(self, Application):
            return False

        abstraction, new_term = self._get_children()
        if Abstraction.check_grammar(abstraction, self.original_expr):
            return abstraction, new_term

        return None, None

    @staticmethod
    def left_outer_redex(expr, original_expr=None):
        """Returns the index path to the leftmost outermost redex, if there is one. The first outermost node is returned
        without checking if it is leftmost, but generally first outermost node == leftmost outermost node.
        """

        def find_outer_redex(term):
            abstraction, new_term = term.is_leftmost()
            if abstraction:
                return term, abstraction, new_term
            for node in term._get_children():
                result = find_outer_redex(LambdaTerm.generate_tree(node, term.original_expr))
                if result:
                    return result

        try:
            redex, abstraction, new_term = find_outer_redex(LambdaTerm.generate_tree(expr, original_expr))
            return redex, abstraction, new_term
        except TypeError:
            return None, None, None

    @staticmethod
    def get_children(expr):
        return LambdaTerm.generate_tree(expr, preprocess=True)._get_children()

    @staticmethod
    def alpha_convert(expr, var, new_arg):
        return LambdaTerm.generate_tree(expr, preprocess=True)._alpha_convert(var, new_arg)

    @staticmethod
    def sub(expr, var, new_term, subscript_cache):
        return LambdaTerm.generate_tree(expr, preprocess=True)._sub(var, new_term, subscript_cache)

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

    @staticmethod
    def get_expr(left, right, original_expr):
        ...

    def _get_children(self):
        ...

    def _alpha_convert(self, var, new_arg):
        if self.expr == var:
            return new_arg
        return self.expr

    def _sub(self, var, new_term, subscript_cache):
        """Beta conversion is similar to alpha conversion for Variables, but types can change."""
        if self.expr == var:
            return new_term
        return self.expr

    @staticmethod
    def subscript(var, num):
        """Returns var with subscript of n."""
        while var[-1] in PureGrammar.SUBS:
            var = var[:-1]  # remove subscripts
        return var + "".join(PureGrammar.SUBS[int(digit)] for digit in PureGrammar.preprocess(str(num)))


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
        arg, body = self._get_children()
        body = LambdaTerm.generate_tree(body, self.original_expr).expr

        self.expr = Abstraction.get_expr(arg, body, self.original_expr)

    @staticmethod
    def get_expr(left, right, original_expr):
        expr = f"λ{left}."
        if Application.check_grammar(right, original_expr, preprocess=False):
            expr += f"({right})"
        else:
            expr += f"{right}"
        return expr

    def _get_children(self):
        return self.expr[self.expr.index("λ") + 1:self.expr.index(".")], self.expr[self.expr.index(".") + 1:]

    def _alpha_convert(self, var, new_arg):
        arg, body = self._get_children()
        if var != arg:
            return Abstraction.get_expr(arg, LambdaTerm.alpha_convert(body, var, new_arg), self.original_expr)
        return self.expr

    def _sub(self, var, new_term, subscript_cache):
        arg, body = self._get_children()
        if var != arg:
            print(f"-> Abstraction._sub:\n    expr: {self.expr}\n")

            if arg in new_term:
                print(f"        old arg: {arg}")
                new_arg = Abstraction.get_new_arg(arg, subscript_cache)
                print(f"        new arg: {new_arg}")

                print(f"        old body: {body}")
                body = LambdaTerm.alpha_convert(body, arg, new_arg)
                arg = LambdaTerm.alpha_convert(arg, arg, new_arg)
                print(f"        new body: {body}\n")

            print(f"    arg: {arg}\n    body: {body}\n"
                  f"    subbed: {Abstraction.get_expr(arg, body, self.original_expr)}\n")
            return Abstraction.get_expr(arg, LambdaTerm.sub(body, var, new_term, subscript_cache), self.original_expr)
        return self.expr

    @staticmethod
    def get_new_arg(arg, subscript_cache):
        """Returns the next term that isn't var nor arg and occurs in neither body nor new_term."""
        max_subscript = -1

        joined_cache = "".join(subscript_cache)
        for idx in range(len(joined_cache) - 1):
            char, next_char = joined_cache[idx], joined_cache[idx + 1]

            if char == arg and next_char in PureGrammar.SUBS:
                subscript = PureGrammar.SUBS.index(next_char)

                if subscript > max_subscript:
                    max_subscript = subscript

        subscripted = Variable.subscript(arg, max_subscript + 1)
        subscript_cache.append(subscripted)

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

        # check 1: are parentheses balanced? (only check if hasn't been preprocessed)
        if preprocess and not PureGrammar.are_parens_balanced(expr):
            raise SyntaxError(template("'{}' has mismatched parentheses", original_expr))

        # check 2: are there spaces or parentheses in expr?
        if " " not in expr and ("(" not in expr or ")" not in expr):
            return False

        # check 3: is expr an Builtin, Variable, or Abstraction?
        if Builtin.check_grammar(expr, original_expr, preprocess=False):
            return False
        elif Variable.check_grammar(expr, original_expr, preprocess=False):
            return False
        elif Abstraction.check_grammar(expr, original_expr, preprocess=False):
            return False
        return True

    def tokenize(self):
        nodes = (LambdaTerm.generate_tree(node, self.original_expr).expr for node in self._get_children())
        self.expr = Application.get_expr(*nodes, self.original_expr)

    @staticmethod
    def get_expr(left, right, original_expr):
        expr = f""
        for node in [left, right]:
            if Variable.check_grammar(node, original_expr, preprocess=False):
                expr += f"{node} "
            else:
                expr += f"({node}) "
        return expr.rstrip()

    def _get_children(self):
        bind_pos = -1
        for idx, char in enumerate(self.expr):
            if char == "λ" and self.expr.count("(", 0, idx + 1) == self.expr.count(")", 0, idx + 1):
                bind_pos = idx
                break

        for idx in range(len(self.expr) - 1, 0, -1):
            to_iterate = self.expr[:idx]

            paren_balance = to_iterate.count("(") == to_iterate.count(")")
            past_bind = bind_pos != -1 and idx > bind_pos
            multichar_var = Variable.check_grammar(self.expr[idx - 1:], self.original_expr)

            initial_tests = paren_balance and not past_bind and not multichar_var

            if initial_tests and LambdaTerm.infer_type(to_iterate, self.original_expr, preprocess=False) is not None:
                left = PureGrammar.preprocess(self.expr[:idx], self.original_expr)
                right = PureGrammar.preprocess(self.expr[idx:], self.original_expr)
                return left, right

        raise SyntaxError(template("'{}' is invalid LambdaTerm grammar", self.original_expr))

    def _alpha_convert(self, var, new_arg):
        nodes = (LambdaTerm.alpha_convert(node, var, new_arg) for node in self._get_children())
        return Application.get_expr(*nodes, self.original_expr)

    def _sub(self, var, new_term, subscript_cache):
        """Beta conversion is applied like alpha conversion for Applications."""
        nodes = (LambdaTerm.sub(node, var, new_term, subscript_cache) for node in self._get_children())
        return Application.get_expr(*nodes, self.original_expr)


class NormalOrderReducer:
    """Implements normal-order beta reduction of a syntax tree. Used instead of LambdaTerm in lang."""

    def __init__(self, expr, reduce=False):
        self.original_expr = expr
        self.expr = LambdaTerm.generate_tree(expr, generate=True).expr

        self.cache = []

        self.reduced = False
        if reduce:
            self.beta_reduce()

    def beta_reduce(self):
        """In-place normal-order beta reduction of self.tree."""
        redex, abstraction, new_term = LambdaTerm.left_outer_redex(self.expr, self.original_expr)

        while redex is not None:
            arg, body = LambdaTerm.get_children(abstraction)
            print(f"{'-' * 80}\n"
                  f"redex: {redex.expr}\nabstraction: {abstraction}\narg: {arg}\nbody: {body}\nnew_term: {new_term}\n"
                  f"{'-' * 80}\n")

            reduced = LambdaTerm.sub(body, arg, new_term, self.cache)
            print(f"{'-' * 80}\nreduced: {reduced}")
            self.expr = LambdaTerm.replace(self.expr, redex.expr, reduced)
            print(f"expr: {self.expr}\n{'-' * 80}\n")

            redex, abstraction, new_term = LambdaTerm.left_outer_redex(self.expr, self.original_expr)

        self.reduced = True

    def __repr__(self):
        return self.expr


if __name__ == "__main__":
    import time

    s = time.time()
    print(NormalOrderReducer("(λx.λy.y x)y", reduce=True))
    print(time.time() - s)
