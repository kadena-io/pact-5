## do

Use `do` to evaluate a sequence of expressions in order and only return the result from the last expression.

### Basic syntax

Use the following syntax:

```pact
(do (my-expression1) (my-expression2) (my-return-expression))
```

### Examples

```pact
pact> (do (print "hello world!") (+ 1 2) (+ 121 299))
"hello world!"
420
```

Notice how the return value is the last addition of `(+ 121 299)`. `do` evaluates every expression supplied, so if any expression errors along the way, the subsequent expressions will never be evaluated, as such:

```pact
pact> (do (enforce false "boom") (+ 1 2))
(interactive):1:4: boom
 1 | (do (enforce false "boom") (+ 1 2))
   |     ^^^^^^^^^^^^^^^^^^^^^^
```
