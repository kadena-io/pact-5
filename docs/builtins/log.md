## log
Use `log` to compute the logarithm of Y with base X.

### Basic syntax

To compute the logarithm of Y with base X, use the following syntax:

log *x y*

### Arguments

Use the following arguments to specify the base (X) and value (Y) for which you want to compute the logarithm using the `log` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| x | integer, decimal | Specifies the base of the logarithm. |
| y | integer, decimal | Specifies the value for which you want to compute the logarithm. |

### Return value

The `log` function returns the logarithm of Y with base X.

### Examples

The following example demonstrates the use of `log` in the Pact REPL:

```lisp
pact>(log 2 256)
8
```

In this example, the logarithm of 256 with base 2 is computed, resulting in 8.