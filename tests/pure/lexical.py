import unittest

from interpreter.pure.lexical import *

class LambdaTermTestCase(unittest.TestCase):

    def test_infer_type(self):
        should_fail = ["aλ", "(())", "(λλa.a)", "(λ)", "((λx.x)) x) y"]
        for case in should_fail:
            print(LambdaTerm.infer_type(case))
            self.assertRaises(ValueError, LambdaTerm.infer_type, case)

        cases = {
            "λ": Builtin("λ"),
            "a": Variable("a"),
            "λx.(x y z)": Abstraction("λx.(x y z)"),
            "(λx.x) a": Application("(λx.x) a"),
        }
        for case, expected in cases.items():
            self.assertEqual(expected, LambdaTerm.infer_type(case), case)


class BuiltinsTestCase(unittest.TestCase):

    def test_check_grammar(self):
        should_fail = ["λa", "awef", "()", ".ew"]
        for case in should_fail:
            self.assertFalse(Builtin.check_grammar(case), case)

        should_pass = ["λ", ".", "(", ")"]
        for case in should_pass:
            self.assertTrue(Builtin.check_grammar(case), case)


class VariableTestCase(unittest.TestCase):

    def test_check_grammar(self):
        should_fail = ["awe3", "6awfe", ".", ".ew", "λx"]
        for case in should_fail:
            self.assertFalse(Variable.check_grammar(case), case)

        should_pass = ["a", "b", "c", "afw", "ba"]
        for case in should_pass:
            self.assertTrue(Variable.check_grammar(case), case)

class AbstractionTestCase(unittest.TestCase):

    def test_check_grammar(self):
        should_fail = ["λxx", ".λx.x", "x.x", "λx[x]", "λx.x λy.y", "λxy.a λab.a(f)(e)xy"]
        for case in should_fail:
            self.assertFalse(Abstraction.check_grammar(case), case)

        should_pass = ["λx.x", "λxy.xy", "λafe.(afe)", "λxy.λab.a(f)(e)xy"]
        for case in should_pass:
            self.assertTrue(Abstraction.check_grammar(case), case)

    def test_step_tokenize(self):
        cases = {
            "λx.λy.λz.x (y z)": [Builtin("λ"), Variable("x"), Builtin("."), Abstraction("λy.λz.x (y z)")],
            "λy.λz.x (y z)": [Builtin("λ"), Variable("y"), Builtin("."), Abstraction("λz.x (y z)")],
            "λz.x(y z)": [Builtin("λ"), Variable("z"), Builtin("."), Application("x(y z)")]
        }

        for case, expected in cases.items():
            self.assertEqual(expected, Abstraction(case).step_tokenize(), case)


class ApplicationTestCase(unittest.TestCase):

    def test_check_grammar(self):
        should_fail = ["xxy", "λx.(λx", "λx.x", "x", "(λx.x))", ")λx.x(", "(λx.λy.(x y z)))((z)λx.x(y))", "aefλ.", "λ."]
        for case in should_fail:
            self.assertFalse(Application.check_grammar(case), case)

        should_pass = ["(λx.x)", "((λx.λz.(y x (x z))) x y ) z a (f n)", "((λx.x (y z)))"]
        for case in should_pass:
            self.assertTrue(Application.check_grammar(case), case)

    def test_step_tokenize(self):
        cases = {
            "(x y) y (x y)": [Application("x y"), Variable("y"), Application("x y")],
            "(z λx.λy.z) (x y)": [Application("z λx.λy.z"), Application("x y")],
            "((z) (λx.λy.z)) ((x) (y))": [Application("(z) (λx.λy.z)"), Application("(x) (y)")],
            "((λx.x) λx.x) λxy.y λabc.a": [Application("(λx.x) λx.x"), Abstraction("λxy.y"), Abstraction("λabc.a")],
            "((λx.x) λx.x) λxy.(y x)": [Application("(λx.x) λx.x"), Abstraction("λxy.y x")],
            "((λx.x) λx.x) λxy.y x  ": [Application("(λx.x) λx.x"), Abstraction("λxy.y x")]
        }
        for case, expected in cases.items():
            self.assertEqual(expected, Application(case).step_tokenize(), case)


if __name__ == '__main__':
    unittest.main()
