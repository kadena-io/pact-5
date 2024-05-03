## sqrt
The `sqrt` function computes the square root of the given value *`X`*.

### Basic syntax

To calculate the square root of a value, use the following syntax:

sqrt *x*

### Arguments

Use the following argument to specify the value for which to compute the square root using the `sqrt` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| x | \<a[integer,decimal]> | Specifies the value for which to compute the square root. |

### Return value

The `sqrt` function returns the square root of the specified value. The return type matches the type of the input value, either an integer or a decimal.

### Examples

The following example demonstrates the usage of the `sqrt` function within a Pact script. It calculates the square root of the value 25:

```lisp
(sqrt 25)
```

This example illustrates how to use the `sqrt` function to compute the square root of a value in Pact, producing either an integer or a decimal result.
