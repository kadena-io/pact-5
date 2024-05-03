Use `exp` to calculate the exponential function of the specified *`X`*.

### Basic syntax

To calculate the exponential function of a value, use the following syntax:

exp *x* -> *result*

### Arguments

Use the following argument to specify the value for the `exp` Pact function:

| Argument | Type             | Description                                 |
|----------|------------------|---------------------------------------------|
| x        | integer or decimal| Specifies the value for which to calculate the exponential function. |

### Return values

The `exp` function returns the exponential function of the specified value.

### Examples

The following example demonstrates the `exp` function:

```lisp
(round (exp 3) 6)
```

In this example, `(exp 3)` is used to calculate the exponential function of 3. The result of this calculation is then rounded to 6 decimal places. The `exp` function provides a way to calculate exponential values in Pact contracts.
