## not

Use `not` to compute the boolean negation of a specified `value`.

### Basic syntax

To compute the boolean negation of the specified `value`, use the following syntax:

```pact
(not value)
```

### Argument

Use the following argument to specify the boolean value for which you want to compute the negation using the `not` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `value` | bool | Specifies the expression to evaluate that returns the boolean value to be negated. |

### Return value

The `not` function returns the boolean negation of the input value.

### Examples

The following example demonstrates how to use of the `not` function in the Pact REPL:

```pact
pact> (not (> 1 2))
true
```

In this example, the expression `(> 1 2)` evaluates to `false`, and the `not` function negates this value, resulting in `true`.
