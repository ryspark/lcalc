# lambda-calculus

Lambda calculus interpreter written in Python, supporting both a file interpreter and a command-line interpreter. 
Supports pure lambda calculus as imagined by Church, but also named functions (`I: λx.x`), `#import`ing of other 
modules, and `#define` directives.

Below is an example program to compute the sum of 50 and 50.

```
#import "common"  ;; load common funcs, including '+'

+ 50 50           ;; will output '100'
``` 

Everything (besides `#import`, `#define`, and named functions) is implemented in pure lambda calculus, making even basic
arithmetic *extremely* slow.

## Syntax

### Lambda calculus syntax

```
<λ-term> ::= <λ-term>                   ; "variable"
                                        ; - must be alphabetic character(s) (by convention, lowercase)
           | "λ" <λ-term> "." <λ-term>  ; "abstraction"
                                        ; - currying is not supported in this implementation (*)
                                        ; - abstraction bodies are greedy: λx.x y = λx.(x y) != (λx.x) (y)
           | <λ-term> <λ-term>          ; "application"
                                        ; - associating by left: abcd = ((((a) b) c) d)
```

(*) See [docstrings](interpreter/pure/lexical.py) for details on why currying isn't supported.

### Language extensions

```
<import_stmt> ::= "#import " <filepath>     ; imports relative to this .lc file ("common" denotes common/*.lc)
<define_stmt> ::= "#define " <char> <char>  ; blindly replaces instances of first <char> with second <char> (*)
                                            ; note that there are a few special cases for the second <char>:
                                            ;  1. <lambda> will replace all λ with first <char> (**)
                                            ;  2. <declare> will replace all := with first <char>

<named_func>  ::= <var> ":=" <λ-term>       ; only reduced if used later on
<exec_stmt>   ::= <λ-term>                  ; will be outputted when interpreter is run

<comment>     ::= ";;" <char>*
```
 
(*) `#define` statements are local to a module; that is, any `#define` directives will not be applied to any lambda
calculus `#import`ed from another module.
 
(**) If length of first `<char>` is greater than one, an extra space is inserted (e.g., `#define lambda <lambda>` tells the
interpreter to replace `lambda x.x` with `λx.x`, rather than replacing `lambdax.x`). Other than this special case, 
both the first and second `<char>`s cannot contain whitespace.
