from interpreter.pure.lexical import LambdaExpr, invariates


def to_church_numeral(number):
    """Natural numbers encoded as Church numerals. Note that operations are not implemented here (see
    common/numerical.lc) and that ChurchNumeral is implemented using lambda calculus syntax, thus keeping
    everything pure as possible.

    Source: https://en.wikipedia.org/wiki/Church_encoding#Calculation_with_Church_numerals
    """
    assert isinstance(number, int) and number >= 0, "only natural numbers supported"

    bind, decl, open_paren, close_paren = invariates()

    header = "{}f{}{}x{}".format(bind, decl, bind, decl)
    composed = "f {}".format(open_paren) * number
    application = "x" + close_paren * composed.count(close_paren)

    return LambdaExpr(header + composed + application)
