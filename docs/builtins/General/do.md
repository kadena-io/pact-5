## do

Use `do` to evaluate a sequence of expressions in order and only return the result from the last expression.

### Basic syntax

To evaluate a series of expressions, use the following syntax:

```pact
(do (my-expression1) (my-expression2) (my-return-expression))
```

### Examples

The following example demonstrates using the do built-in function to evaluate three expressions using the Pact command-line interpreter interactively:

```pact
pact> (do (base64-encode "hello world!") (+ 1 2) (+ 121 299))
420
```

Notice that this example returns the value from the last expression evaluated, in this case, the result of the `(+ 121 299)` expression. 
You should also note that the `do` built-in function evaluates every expression in order. 
If any expression in the sequence results in an error, the remaining expressions are never evaluated.
For example, if the first expression results in a error, only the error is returned:

```pact
pact> (do (enforce false "boom") (+ 1 2))
(interactive):1:4: boom
 1 | (do (enforce false "boom") (+ 1 2))
   |     ^^^^^^^^^^^^^^^^^^^^^^
```

