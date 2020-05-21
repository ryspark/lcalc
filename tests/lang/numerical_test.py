import unittest

from interpreter.lang import numerical
from interpreter.pure.lexical import LambdaExpr

class NumericalTestCase(unittest.TestCase):

    def test_to_church_numeral(self):
        should_fail = [-2, 0.3, 4.0, 14.2]
        for case in should_fail:
            self.assertRaises(AssertionError, numerical.to_church_numeral, number=case)

        should_pass = {0: "λf.λx.x", 3: "λf.λx.f (f (f (x)))"}
        for case, result in should_pass.items():
            self.assertEqual(LambdaExpr(result), numerical.to_church_numeral(case))


if __name__ == '__main__':
    unittest.main()
