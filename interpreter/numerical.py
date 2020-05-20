from dataclasses import dataclass

from interpreter import term


@dataclass(init=False)
class ChurchNumeral:
    """Natural numbers encoded as Church numerals. Note that operations are not implemented here- see
    common/numerical.lc. Note that ChurchNumeral is implemented using lc syntax, thus keeping everything pure as
    possible.

    Source: https://en.wikipedia.org/wiki/Church_encoding#Calculation_with_Church_numerals

    """
    number: int
    term: term.LambdaTerm

    def __init__(self, number):
        """Initializes a ChurchNumeral

        :param number: natural number
        """
        assert isinstance(number, int) and number >= 0, "only natural numbers supported"
        self.number = number

        lambda_term = "λf.λx."
        for _ in range(number):
            lambda_term += "f ("
        lambda_term += "x" + ")" * lambda_term.count("(")
        self.term = term.LambdaTerm(lambda_term)
