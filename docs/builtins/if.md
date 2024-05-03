## if
Use `if` to test a condition. If the condition is true, evaluate the *`then`* expression; otherwise, evaluate the *`else`* expression.

### Basic syntax

To test a condition and execute different expressions based on the result, use the following syntax:

if *condition then else*

### Arguments

Use the following arguments to define the condition and expressions to be evaluated based on the outcome of the condition using the `if` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| cond | boolean | Specifies the condition to be tested. |
| then | any | Specifies the expression to be evaluated if the condition is true. |
| else | any | Specifies the expression to be evaluated if the condition is false. |

### Return value

The `if` function returns the result of evaluating either the *`then`* expression or the *`else`* expression, depending on the outcome of the condition.

### Examples

The following example demonstrates the use of `if` to test a condition in the Pact REPL:

```lisp
pact>(if (= (+ 2 2) 4) "Sanity prevails" "Chaos reigns")
"Sanity prevails"
```

In this example, the condition `(= (+ 2 2) 4)` evaluates to true, so the expression `"Sanity prevails"` is returned.

```lisp
pact>(if (= (+ 2 2) 5) "Sanity prevails" "Chaos reigns")
"Chaos reigns"
```

In this example, the condition `(= (+ 2 2) 5)` evaluates to false, so the expression `"Chaos reigns"` is returned.
