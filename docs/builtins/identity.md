Use `identity` to return the provided value.

### Basic syntax

To return the provided value, use the following syntax:

identity *value*

### Arguments

Use the following argument to specify the value you want to return using the `identity` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| value | any | Specifies the value to be returned. |

### Return value

The `identity` function returns the provided value.

### Examples

The following example demonstrates the use of `identity` within a `map` function in the Pact REPL:

```lisp
pact>(map (identity) [1 2 3])
[1 2 3]
```

This example applies the `identity` function to each element in the list `[1, 2, 3]`, effectively returning the same list.
