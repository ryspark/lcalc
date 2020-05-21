import unittest

from interpreter.pure.lexical import LambdaVar, LambdaExpr, LambdaAST

class LambdaTermTestCase(unittest.TestCase):

    def test_LambdaVar(self):
        should_fail = ["awe3", "6awfe", ".", ".ew", "λx"]
        for case in should_fail:
            self.assertFalse(LambdaVar.check_grammar(case), case)

        should_pass = ["a", "bawef", "aE"]
        for case in should_pass:
            self.assertTrue(LambdaVar.check_grammar(case), case)

    def test_LambdaExpr(self):
        should_fail = ["λxx", ".λx.x", "x.x", "λx[x]"]
        for case in should_fail:
            self.assertFalse(LambdaExpr.check_grammar(case))

        should_pass = ["λx.x", "λxy.xy", "λafe.λxy.a(f)(e)xy"]
        for case in should_pass:
            self.assertTrue(LambdaExpr.check_grammar(case))


class LambdaASTTestCase(unittest.TestCase):

    def test_tokenize(self):
        should_fail = ["λx.(λx", ")λx.x", "(λx.(x)", "(x)λx.x)"]
        for case in should_fail:
            self.assertRaises(AssertionError, LambdaAST.tokenize, case)

        should_pass = {
            "(x y) y (x y)": ["x y", "y", "x y"],
            "(z λx.λy.z) (x y)": ["z λx.λy.z", "x y"],
            "((z) (λx.λy.z)) ((x) (y))": ["(z) (λx.λy.z)", "(x) (y)"],
            "((λx.x) λx.x) λxy.y λabc.a": ["(λx.x) λx.x", "λxy.y", "λabc.a"]
        }
        for case, result in should_pass.items():
            self.assertEqual(result, LambdaAST.tokenize(case))


if __name__ == '__main__':
    unittest.main()
