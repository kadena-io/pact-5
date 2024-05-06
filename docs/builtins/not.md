## not

Use `not` to compute the boolean negation of a value.

### Basic syntax

To compute the boolean negation of `X`, use the following syntax:

`(not X)`

### Argument

Use the following argument to specify the boolean value for which you want to compute the negation using the `not` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `X` | `bool` | Specifies the boolean value to be negated. |

### Return value

The `not` function returns the boolean negation of the input value.

### Examples

The following example demonstrates the use of `not` in the Pact REPL:

```lisp
pact>(not (> 1 2))
true
```

In this example, the expression `>(1, 2)` evaluates to `false`, and `not` negates this value, resulting in `true`.
