# lambda-calculus

Lambda calculus interpreter written in Python, supporting both a file interpreter and a command-line interpreter. 
Supports pure lambda calculus as imagined by Church, but also named functions (`I: λx.x`), `#import`ing of other 
modules, and `#define` directives.

Below is an example program to compute the sum of 50 and 50.

```
#import "common" -- load '+' function

+ 50 50 -- will output '100'
``` 

Everything (besides `#import`, `#define`, and named functions) is implemented in pure lambda calculus, making even basic
arithmetic *extremely* slow.
