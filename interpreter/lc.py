"""Lambda calculus (lc) interpreter.

Writing this without any knowledge of how modern compilers/interpreters do their jobs, so it's bound to be pretty
awful :). Basic program flow:
    1. Parser: produces an AST
    2. Semantic analysis: uh
    3. Code generation: not a compiler, so will just load (python) instructions and execute on the fly

*AST based off implementation here: http://pages.cs.wisc.edu/~horwitz/CS704-NOTES/1.LAMBDA-CALCULUS.html

"""
