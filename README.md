# lambda-calculus

Lambda calculus interpreter written in Python, supporting both a file interpreter and a command-line interpreter. 
Supports pure lambda calculus as imagined by Church, but also named functions (`I: λx.x`), `#import` statements, and 
`#define statements`.

Below is an example program to compute the sum of 50 and 50.

```
#import "common" -- load '+' function

+ 50 50 -- will output '100'
``` 

Everything (besides `#import`, `#define`, and named functions) is implemented in pure lambda calculus, making even basic
arithmetic *extremely* slow.

## To-do:
1. String-based and functional-based pure lambda calculus beta-reduction
2. Faster termination for exprs with no beta-normal form
3. Cache for AST generation/beta-reduction
4. Fibonacci in `examples`
