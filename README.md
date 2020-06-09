# lcalc

A lambda calculus interpreter written in Python, supporting both a file interpreter and a command-line interpreter. 
Supports pure lambda calculus as imagined by Church, but also named functions (`I := λx.x`), `#import`ing of other 
modules, and `#define` directives. Made solely for fun during quarantine!

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

(*) Why is currying not supported? Because it makes the use of multi-character Variables ambiguous. For example, if
currying is allowed, what does the expression `λvar.x` mean? Should it be resolved to `λv.λa.λr.x`, or is `var` a
Variable name? Thus, currying and multi-character Variable cannot coexist without causing ambiguity. This
implementation favors multi-character Variables over currying, a purely arbitrary decision. Relatedly, this feature
means that function application must be separated by spaces or parentheses.

### Language extensions

```
<import_stmt> ::= "#import " <filepath>     ; imports relative to this .lc file ("common" -> common/*.lc)
<define_stmt> ::= "#define " <char> <char>  ; blindly replaces instances of 1st <char> with 2nd <char> (*)
                                            ; note that there are a few special cases for the second <char>:
                                            ;  1. <lambda> will replace all λ with first <char> (**)
                                            ;  2. <declare> will replace all := with first <char>

<named_func>  ::= <var> ":=" <λ-term>       ; only reduced if used later on
<exec_stmt>   ::= <λ-term>                  ; will be outputted when interpreter is run

<comment>     ::= ";;" <char>*
```
 
(*) `#define` statements are local to a module; that is, any `#define` directives will not be applied to any lambda
calculus `#import`ed from another module.
 
(**) If length of first `<char>` is greater than one, an extra space is inserted (e.g., `#define lambda <lambda>` tells 
the interpreter to replace `lambda x.x` with `λx.x`, rather than replacing `lambdax.x`). Other than this special case, 
both the first and second `<char>`s cannot contain whitespace.

## Usage

To install, add a symbolic link from `interpreter/lcalc` to `/usr/local/bin/lcalc`.

### Command-line mode

Run `lcalc [args]` to enter interactive interpreter mode.

### File interpreter mode

Run `lcalc [args] <filename>.lc` to interpret and execute a `.lc` file. Note that evaluation (i.e., beta reduction) is 
lazy, and all statements that are not `#import`s, `#define`s, or bindings (`NAME := <λ-term>`) will be executed.

### Optional arguments

1. `-s`, `--sub`: if used, this flag turns off automatic reverse substitution of named statements into beta-reduced 
λ-term. This flag will not turn off automatic conversion of Church numerals to numbers. Note that Church numerals take 
precedence over named statements; that is, `λf.x.x` -> `0`, not `FALSE`.

See [`examples`](examples) for sample `.lc` programs.
