import unittest

from interpreter.pure.lexical import Abstraction, Builtin, Application, LambdaAST, LambdaTerm, Variable

class BuiltinTestCase(unittest.TestCase):

    def test_check_grammar(self):
        should_raise = ["λ.", "()", ".("]
        for case in should_raise:
            self.assertRaises(SyntaxError, Builtin.check_grammar, case)

        should_fail = ["awef", "λx.x", "(a) (b)", "((a) (b))"]
        for case in should_fail:
            self.assertFalse(Builtin.check_grammar(case), case)

        should_pass = ["λ", ".", "(", ")"]
        for case in should_pass:
            self.assertTrue(Builtin.check_grammar(case), case)


class LambdaTermTestCase(unittest.TestCase):

    def test_infer_type(self):
        should_fail = ["aλ", "", "λ", "((λx.x)) x) y", ".", "λ.", "λλx.x", "λλx"]
        for case in should_fail:
            self.assertRaises(SyntaxError, LambdaTerm.infer_type, case)

        cases = {
            "λx.x": Abstraction("λx.x"),
            "a": Variable("a"),
            "λx.(x y z)": Abstraction("λx.(x y z)"),
            "(λx.x) a": Application("(λx.x) a"),
            "(λx.x)": Application("(λx.x)"),
        }
        for case, expected in cases.items():
            self.assertEqual(expected, LambdaTerm.infer_type(case), case)


class VariableTestCase(unittest.TestCase):

    def test_check_grammar(self):
        should_raise = [""]
        for case in should_raise:
            self.assertRaises(SyntaxError, Variable.check_grammar, case)

        should_fail = ["awe3", "6awfe", ".", ".ew", "λx"]
        for case in should_fail:
            self.assertFalse(Variable.check_grammar(case), case)

        should_pass = ["a", "b", "c", "afw", "ba"]
        for case in should_pass:
            self.assertTrue(Variable.check_grammar(case), case)

class AbstractionTestCase(unittest.TestCase):

    def test_check_grammar(self):
        should_raise = [".λx.x", "λx.", "λxx", "λx[x]", "x.x", "λλx.x"]
        for case in should_raise:
            self.assertRaises(SyntaxError, Abstraction.check_grammar, case)

        should_fail = ["(λx.x) (x)", "(x y) (λcab.cab)", "x y"]
        for case in should_fail:
            self.assertFalse(Abstraction.check_grammar(case), case)

        should_pass = ["λx.x", "((λx.x (y z)))", "λxy.λab.a(f)(e)xy", "λx.(λy.y)", "λx.x λy.y", "λxy.a λab.a(f)(e)xy"]
        for case in should_pass:
            self.assertTrue(Abstraction.check_grammar(case), case)

    def test_step_tokenize(self):
        cases = {
            "λx.λy.λz.x (y z)": [Variable("x"), Abstraction("λy.λz.x (y z)")],
            "λy.λz.x (y z)": [Variable("y"), Abstraction("λz.x (y z)")],
            "λz.x(y z)": [Variable("z"), Application("x(y z)")],
            "λxy.y λabc.a": [Variable("xy"), Application("y λabc.a")],
            "λxy.a λab.a(f)(e)xy": [Variable("xy"), Application("a λab.a(f)(e)xy")]
        }

        for case, expected in cases.items():
            abstraction = Abstraction(case)
            abstraction.step_tokenize()
            self.assertEqual(expected, abstraction.nodes, case)


class ApplicationTestCase(unittest.TestCase):

    def test_check_grammar(self):
        should_raise = ["λx.(λx", "(λx.x))", ")λx.x(", "(λx.λy.(x y z)))((z)λx.x(y))"]
        for case in should_raise:
            self.assertRaises(SyntaxError, Application.check_grammar, case)

        should_fail = ["xy", "λx.x", "x", "λxy.y λabc.a", "(λx.x)", "((λx.x (y z)))", "(λx.x)"]
        for case in should_fail:
            self.assertFalse(Application.check_grammar(case), case)

        should_pass = ["((λx.λz.(y x (x z))) x y ) z a (f n)", "x y", "(x) y", "(x y)", "((x y))", "a (λx. x)k (y)"]
        for case in should_pass:
            self.assertTrue(Application.check_grammar(case), case)

    def test_step_tokenize(self):
        should_raise = ["(λx.x) . λx.x", "(λx.x)λ.", "(λ.x) (λx.x)", "(((x)) λx.)", "(λ)(x.)"]
        for case in should_raise:
            self.assertRaises(SyntaxError, Application(case).step_tokenize)

        cases = {
            "x y": [Variable("x"), Variable("y")],
            "(x) y": [Variable("x"), Variable("y")],
            "x (y)": [Variable("x"), Variable("y")],
            "x(λx.y)": [Variable("x"), Abstraction("λx.y")],
            "x λx.y": [Variable("x"), Abstraction("λx.y")],
            "(x y) y (x y)": [Application("x y"), Variable("y"), Application("x y")],
            "(z λx.λy.z) (x y)": [Application("z λx.λy.z"), Application("x y")],
            "((z) (λx.λy.z)) ((x) (y))": [Application("(z) (λx.λy.z)"), Application("(x) (y)")],
            "((λx.x) λx.x) λxy.y λabc.a": [Application("(λx.x) λx.x"), Abstraction("λxy.y λabc.a")],
            "(x) (λxy.(λx.(y x)) λa.λx.y x)": [Variable("x"), Abstraction("λxy.(λx.(y x)) λa.λx.y x")],
            "((λx.x) λx.x) (λxy.y x)": [Application("(λx.x) λx.x"), Abstraction("λxy.y x")],
            "((λx.x) λx.x) (λxy.(y x))": [Application("(λx.x) λx.x"), Abstraction("λxy.(y x)")],
            "((λx.x) λx.x) (λxy.(λx.(y x)))": [Application("(λx.x) λx.x"), Abstraction("λxy.(λx.(y x))")],
            "((λx.x) λx.x)λx.(λxy.y x)": [Application("(λx.x) λx.x"), Abstraction("λx.(λxy.y x)")],
            "((λx.x) λx.x) λx. (λxy.y x)": [Application("(λx.x) λx.x"), Abstraction("λx. (λxy.y x)")],
            "((λx.x) λx.x) λx.(λxy.y x)": [Application("(λx.x) λx.x"), Abstraction("λx.(λxy.y x)")],
            "((λx.x) λx.x)λx.(λxy.y x (λxy.y x))": [Application("(λx.x) λx.x"), Abstraction("λx.(λxy.y x (λxy.y x))")],
            "((λx.x) λx.x)λx. (λxy.y x)λx.(x)": [Application("(λx.x) λx.x"), Application("λx. (λxy.y x)λx.(x)")],
        }
        for case, expected in cases.items():
            application = Application(case)
            application.step_tokenize()
            self.assertEqual(expected, application.nodes, case)


class LambdaASTTestCase(unittest.TestCase):

    def test_left_outer_redex(self):
        # NOTE: also implicitly tests generate_tree
        should_fail = [LambdaAST("λx.x"), LambdaAST("(x y)"), LambdaAST("x ((x y)) λx.x"), LambdaAST("x (λx.x)")]
        for case in should_fail:
            self.assertIsNone(case.left_outer_redex(), case)

        should_pass = {
            LambdaAST("(λz.((λy.z (v y)) λy.(y v)) λv.z)"): Application("(λy.z (v y)) λy.(y v)"),
            LambdaAST("(y ((λv.λu.z (λu.u (y y))) λy.y))"): Application("((λv.λu.z (λu.u (y y))) λy.y)"),
            LambdaAST("(λv.(λz.z (v λv.v)) λv.y)"): Application("(λz.z (v λv.v)) λv.y"),
            LambdaAST("((λx.λv.v (λu.(v u) λz.λu.y)) v)"): Application("(λx.λv.v (λu.(v u) λz.λu.y)) v"),
            LambdaAST("(λz.(λz.y (y z)) (λv.v x))"): Application("(λz.y (y z)) (λv.v x)")
        }
        for case, result in should_pass.items():
            self.assertEqual(result, case.left_outer_redex(), case)


if __name__ == '__main__':
    unittest.main()
