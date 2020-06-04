"""Natural numbers encoded as Church numerals. Note that operations are not implemented here (see
common/numerical.lc) and that ChurchNumeral is implemented using lambda calculus syntax, thus keeping
everything pure as possible.

Source: https://en.wikipedia.org/wiki/Church_encoding#Calculation_with_Church_numerals
"""

from lang.error import template
from pure.lexical import Abstraction, Variable


def cnumber(num):
    """Returns string expr of num in lambda calculus (cnum = Church numeral)."""
    try:
        assert not isinstance(num, float)
        num = int(num)
        assert num >= 0
    except (AssertionError, ValueError):
        raise ValueError(template("expected natural number, got '{}')", num, internal=True))

    header = "(λf.λx."
    composed = "f (" * num
    application = "x" + ")" * composed.count("(") + ")"

    return Abstraction(header + composed + application)


def number(cnum):
    """Returns str(number) given LambdaTerm cnum. If cnum isn't a Church numeral, returns None."""
    try:
        first_arg, first_body = cnum.nodes
        second_arg, nth_body = first_body.nodes
    except ValueError:
        return None

    num = 0
    while nth_body.tokenizable:
        var, nth_body = nth_body.nodes

        if var.tokenizable or not var == first_arg:
            return None
        num += 1

    return str(num) if nth_body == second_arg else None


def cnumberify(nor):
    """In-place replacement all numbers in NormalOrderReducer with cnumbers."""
    for node, paths in nor.flattened.items():
        if node.expr.isdigit():
            for path in paths:
                nor.set(path, cnumber(node.expr))


def numberify(nor):
    """In-place replacement all cnumbers in NormalOrderReducer with numbers."""

    def _numberify(nor, node, path):
        num = number(node)
        if num:
            nor.set(path, Variable(num))
        else:
            for idx, sub_node in enumerate(node.nodes):
                _numberify(nor, sub_node, path + [idx])

    _numberify(nor, nor.tree, [])
