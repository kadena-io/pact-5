Use `mod` to compute the remainder of X divided by Y.

### Basic syntax

To compute the remainder of X divided by Y, use the following syntax:

mod *x y*

### Arguments

Use the following arguments to specify the integers for which you want to compute the remainder using the `mod` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| x | integer | Specifies the dividend. |
| y | integer | Specifies the divisor. |

### Return value

The `mod` function returns the remainder of the division of X by Y.

### Examples

The following example demonstrates the use of `mod` in the Pact REPL:

```lisp
pact>(mod 13 8)
5
```

In this example, the remainder of 13 divided by 8 is computed, resulting in 5.
