"""Natural numbers encoded as Church numerals. Note that operations are not implemented here (see
common/numerical.lc) and that ChurchNumeral is implemented using lambda calculus syntax, thus keeping
everything pure as possible.

Source: https://en.wikipedia.org/wiki/Church_encoding#Calculation_with_Church_numerals
"""


def church_numeral(number):
    """Returns string expr of number in lambda calculus."""
    assert isinstance(number, int) and number >= 0, "only natural numbers supported"

    header = "(λf.λx."
    composed = "f (" * number
    application = "x" + ")" * composed.count("(") + ")"

    return header + composed + application
