## or

Use `or` for boolean logic with short-circuit evaluation.

### Basic syntax

To perform boolean logic with short-circuit evaluation, use the following syntax:

`(or x y)`

### Arguments

Use the following arguments to specify the boolean values for which you want to perform the logical OR operation using the `or` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `x` | `bool` | Specifies the first boolean value. |
| `y` | `bool` | Specifies the second boolean value. |

### Return value

The `or` function returns a boolean value based on the logical OR operation of the input values.

### Examples

The following example demonstrates the use of `or` in the Pact REPL:

```pact
pact>(or true false)
true
```

In this example, the logical OR operation is performed between `true` and `false`, resulting in `true`.
