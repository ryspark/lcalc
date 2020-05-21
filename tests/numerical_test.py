import unittest

from interpreter import term
from interpreter.lang import numerical


class NumericalTestCase(unittest.TestCase):

    def test_num_to_lc(self):
        should_fail = [-2, 0.3, 4.0, 14.2]
        for case in should_fail:
            self.assertRaises(AssertionError, numerical.ChurchNumeral, number=case)

        success_tests = {0: "λf.λx.x", 3: "λf.λx.f (f (f (x)))"}
        for case, result in success_tests.items():
            self.assertEqual(numerical.ChurchNumeral(case).term, term.LambdaTerm(result))


if __name__ == '__main__':
    unittest.main()
