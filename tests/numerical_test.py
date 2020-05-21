import unittest

from interpreter.lang import numerical
from interpreter.pure.lexical import LambdaExpr

class NumericalTestCase(unittest.TestCase):

    def test_num_to_lc(self):
        should_fail = [-2, 0.3, 4.0, 14.2]
        for case in should_fail:
            self.assertRaises(AssertionError, numerical.to_church_numeral, number=case)

        success_tests = {0: "λf.λx.x", 3: "λf.λx.f (f (f (x)))"}
        for case, result in success_tests.items():
            self.assertEqual(numerical.to_church_numeral(case), LambdaExpr(result))


if __name__ == '__main__':
    unittest.main()
