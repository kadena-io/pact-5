The `!=` function returns true if the first argument `x` does not equal the second argument `y`.

### Basic syntax

To check if `x` does not equal `y`, use the following syntax:

!= *x* *y*

### Arguments

Use the following arguments to specify the values for comparison using the `!=` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| x | <a[integer, string, time, decimal, bool, [<l>], object:<{o}>, keyset, guard, module{}]> | Specifies the first value for comparison. |
| y | <a[integer, string, time, decimal, bool, [<l>], object:<{o}>, keyset, guard, module{}]> | Specifies the second value for comparison. |

### Return value

The `!=` function returns true if `x` does not equal `y`, otherwise false.

### Examples

The following example demonstrates the usage of the `!=` function within a Pact script. It checks if two strings are not equal:

```lisp
(!= "hello" "goodbye")
```

This example illustrates how to use the `!=` function to compare values for inequality in Pact, allowing for conditional logic based on whether two values are not equal.
