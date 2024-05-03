## ceiling
Use `ceiling` to round up the value of a decimal *`x`* to the nearest integer or to a specified precision *`prec`* as a decimal.

### Basic syntax

To round up the value of a decimal *`x`* to the nearest integer, use the following syntax:

ceiling *x*

To round up the value of a decimal *`x`* to a specified precision *`prec`* as a decimal, use the following syntax:

ceiling *x* *prec*

### Arguments

Use the following arguments to specify the decimal *`x`* and optional precision *`prec`* for the `ceiling` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| x | decimal | Specifies the decimal value to round up. |
| prec | integer | (Optional) Specifies the precision to which to round the *`x`* value. If not provided, *`x`* is rounded up to the nearest integer. |

### Return values

The `ceiling` function returns the rounded-up value of *`x`* as either an integer or a decimal based on the input and precision.

### Examples

The following example rounds up a decimal value to the nearest integer in the Pact REPL:

```lisp
pact>(ceiling 3.5)
4
```

In this example, `ceiling` rounds up the decimal value `3.5` to the nearest integer, resulting in `4`.

The following example rounds up a decimal value to a precision of 2 decimal places:

```lisp
pact>(ceiling 100.15234 2)
100.16
```

In this example, `ceiling` rounds up the decimal value `100.15234` to a precision of 2 decimal places, resulting in `100.16`.
