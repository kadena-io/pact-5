The `round` function performs Banker's rounding, returning either an integer value if no precision is specified or a decimal value with the specified precision.

### Basic syntax

To round a decimal value to the nearest integer, use the following syntax:

round *x*

To round a decimal value to a specified precision, use the following syntax:

round *x* *prec*

### Arguments

Use the following arguments to specify the value to be rounded and, optionally, the precision to round to using the `round` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| x | decimal | Specifies the decimal value to be rounded. |
| prec | integer | (Optional) Specifies the precision to round to, if applicable. |

### Return value

If no precision is specified, the `round` function returns the rounded value as an integer. If precision is specified, it returns the rounded value as a decimal.

### Examples

The following examples demonstrate the usage of the `round` function within a Pact script.

To round the decimal value 3.5 to the nearest integer:

```lisp
(round 3.5)
```

To round the decimal value 100.15234 to 2 decimal places:

```lisp
(round 100.15234 2)
```

These examples illustrate how to use the `round` function to perform Banker's rounding on decimal values in Pact, either to the nearest integer or to a specified precision.
