import unittest

from interpreter.pure.lexical import Variable, Abstraction, LambdaAST

class LambdaTermTestCase(unittest.TestCase):

    def test_Variable(self):
        should_fail = ["awe3", "6awfe", ".", ".ew", "λx"]
        for case in should_fail:
            self.assertFalse(Variable.check_grammar(case), case)

        should_pass = ["a", "bawef", "aE"]
        for case in should_pass:
            self.assertTrue(Variable.check_grammar(case), case)

    def test_Abstraction(self):
        should_fail = ["λxx", ".λx.x", "x.x", "λx[x]", "λxy.λab.a(f)(e)xy"]
        for case in should_fail:
            self.assertFalse(Abstraction.check_grammar(case), case)

        should_pass = ["λx.x", "λxy.xy", "λafe.(afe)"]
        for case in should_pass:
            self.assertTrue(Abstraction.check_grammar(case), case)


class LambdaASTTestCase(unittest.TestCase):

    def test_step_tokenize(self):
        should_fail = ["λx.(λx", ")λx.x", "(λx.(x)", "(x)λx.x)"]
        for case in should_fail:
            self.assertRaises(AssertionError, LambdaAST.step_tokenize, case)

        should_pass = {
            "(x y) y (x y)": ["x y", "y", "x y"],
            "(z λx.λy.z) (x y)": ["z λx.λy.z", "x y"],
            "((z) (λx.λy.z)) ((x) (y))": ["(z) (λx.λy.z)", "(x) (y)"],
            "((λx.x) λx.x) λxy.y λabc.a": ["(λx.x) λx.x", "λxy.y", "λabc.a"],
            # "((λx.x) λx.x) λxy.y x": ["(λx.x) λx.x", "λxy.y x"]
        }
        for case, result in should_pass.items():
            self.assertEqual(result, LambdaAST.step_tokenize(case))


if __name__ == '__main__':
    unittest.main()
