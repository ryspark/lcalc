import unittest

from interpreter.pure.lexical import Abstraction, Application, Builtin, LambdaTerm, NormalOrderReducer, Variable

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

    def test_generate_tree(self):
        should_fail = ["aλ", "", "λ", "((λx.x)) x) y", ".", "λ.", "λλx.x", "λλx"]
        for case in should_fail:
            self.assertRaises(SyntaxError, LambdaTerm.generate_tree, case)

        cases = {
            "λx.x": Abstraction("λx.x"),
            "a": Variable("a"),
            "λx.(x y z)": Abstraction("λx.(x y z)"),
            "(λx.x) a": Application("(λx.x) a"),
            "(λx.x)": Abstraction("(λx.x)"),
        }
        for case, expected in cases.items():
            self.assertEqual(expected, LambdaTerm.generate_tree(case), case)

    def test_left_outer_redex(self):
        should_fail = [
            LambdaTerm.generate_tree("λx.x"),
            LambdaTerm.generate_tree("(x y)"),
            LambdaTerm.generate_tree("x ((x y)) λx.x"),
            LambdaTerm.generate_tree("x (λx.x)")
        ]
        for case in should_fail:
            self.assertIsNone(case.left_outer_redex(), case)

        cases = {
            "(λz.((λy.z (v y)) λy.(y v)) λv.z)": LambdaTerm.generate_tree("(λy.z (v y)) λy.(y v)"),
            "(y ((λv.λu.z (λu.u (y y))) λy.y))": LambdaTerm.generate_tree("((λv.λu.z (λu.u (y y))) λy.y)"),
            "(λv.(λz.z (v λv.v)) λv.y)": LambdaTerm.generate_tree("(λz.z (v λv.v)) λv.y"),
            "((λx.λv.v (λu.(v u) λz.λu.y)) v)": LambdaTerm.generate_tree("(λx.λv.v (λu.(v u) λz.λu.y)) v"),
            "(λz.(λz.y (y z)) (λv.v x))": LambdaTerm.generate_tree("(λz.y (y z)) (λv.v x)")
        }
        for case, result in cases.items():
            self.assertEqual(result, LambdaTerm.generate_tree(case).left_outer_redex(), case)


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

    def test_init(self):
        cases = {
            "λx.λy.λz.x (y z)": [Variable("x"), Abstraction("λy.λz.x (y z)")],
            "λy.λz.x (y z)": [Variable("y"), Abstraction("λz.x (y z)")],
            "λz.x(y z)": [Variable("z"), Application("x(y z)")],
            "λxy.y λabc.a": [Variable("xy"), Application("y λabc.a")],
            "λxy.a λab.a(f)(e)xy": [Variable("xy"), Application("a λab.a(f)(e)xy")]
        }

        for case, expected in cases.items():
            self.assertEqual(expected, Abstraction(case).nodes, case)


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

    def test_init(self):
        should_raise = ["(λx.x) . λx.x", "(λx.x)λ.", "(λ.x) (λx.x)", "(((x)) λx.)", "(λ)(x.)", "xλ.x"]
        for case in should_raise:
            self.assertRaises(SyntaxError, Application, case)

        cases = {
            "x y": [Variable("x"), Variable("y")],
            "(x) y": [Variable("x"), Variable("y")],
            "x (y)": [Variable("x"), Variable("y")],
            "x(λx.y)": [Variable("x"), Abstraction("λx.y")],
            "x λx.y": [Variable("x"), Abstraction("λx.y")],
            "(λx.x)y x": [Abstraction("λx.x"), Application("y x")],
            "(x y) y (x y)": [Application("x y"), Application("y (x y)")],
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
            "((λx.x) λx.x)λx. (λxy.y x)λx.(x)": [Application("(λx.x) λx.x"), Abstraction("λx. (λxy.y x)λx.(x)")],
        }
        for case, expected in cases.items():
            self.assertEqual(expected, Application(case).nodes, case)


class NormalOrderReducerTestCase(unittest.TestCase):

    def test_alpha_reduce(self):
        should_fail = ["(λx.x λy.y x)", "λx.(λy.λz.y)", "λx.λy.y"]
        for case in should_fail:
            nor = NormalOrderReducer(case)
            self.assertRaises(ValueError, nor.alpha_convert, Variable("x"), Variable("y"))

        cases = {
            "λx.x": "(λy.y)",
            "λx.(x λa.a)": "(λy.(y (λa.a)))",
            "λx.(λa.(b c) x)": "(λy.(λa.((b c) y)))",
            "λx.(x λx.x)": "(λy.(y (λx.x)))",
            "λx.((λb.x(v b)) λb.(b v x)) λv.x": "(λy.(((λb.(y (v b))) (λb.(b (v y)))) (λv.y)))",
            "λx.((λb.λx.x(v b)) λb.(b v x)) λv.x": "(λy.(((λb.(λx.(x (v b)))) (λb.(b (v y)))) (λv.y)))"
        }
        for case, result in cases.items():
            nor = NormalOrderReducer(case)
            nor.alpha_convert(Variable("x"), Variable("y"))
            self.assertEqual(result, nor.expr, case)


if __name__ == '__main__':
    unittest.main()
