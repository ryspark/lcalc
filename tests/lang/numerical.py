import unittest

from interpreter.lang.numerical import cnumber, number
from interpreter.pure.lexical import Abstraction


class NumericalTestCase(unittest.TestCase):

    def test_cnumber(self):
        should_fail = [-2, 0.3, 4.0, 14.2]
        for case in should_fail:
            self.assertRaises(ValueError, cnumber, case)

        should_pass = {0: Abstraction("λf.λx.x"), 3: Abstraction("λf.λx.f (f (f (x)))")}
        for case, result in should_pass.items():
            self.assertEqual(result, cnumber(case))

    def test_number(self):
        should_fail = [Abstraction("λf.λx.f f"), Abstraction("λf.λx.x f x")]
        for case in should_fail:
            self.assertEqual(number(case), case)

        should_pass = {3: Abstraction("λf.λx.f (f (f (x)))"), 0: Abstraction("λf.λx.x")}
        for result, case in should_pass.items():
            self.assertEqual(str(result), number(case), case)


if __name__ == '__main__':
    unittest.main()
