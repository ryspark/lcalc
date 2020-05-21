"""Lambda calculus interpreter.

For reference:
- "Pure lambda calculus": lambda calculus as defined by Church
- "lc language": lambda calculus + syntax used in .lc files

Writing this without much knowledge of how modern compilers/interpreters do their jobs, so it's bound to be pretty
awful :). Basic program flow:
    1. Parser: produces a lambda calculus AST by recursively tokenizing each statement
        - For pure lambda calculus syntax/grammar rules, see interpreter/grammer/pure.py
        - For the lc language grammar rules, see interpreter/grammar/lang.py
    2. Semantic analysis: walks the produced AST to ensure
        - Will fail if there is an invalid statement
    3. Code generation: not a compiler, so will just load (python) instructions and execute on the fly

"""
