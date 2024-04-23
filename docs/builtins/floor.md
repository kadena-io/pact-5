Use `floor` to round down the value of a decimal *`X`* to an integer, or to a specified precision *`PREC`* as a decimal.

## Basic syntax

To round down a decimal value to an integer or to a specified precision, use the following syntax:

floor *x* -> *result*
floor *x* *prec* -> *result*

## Arguments

Use the following arguments to specify the decimal value and precision for the `floor` Pact function:

| Argument | Type     | Description                                          |
|----------|----------|------------------------------------------------------|
| x        | decimal  | Specifies the decimal value to round down.           |
| prec     | integer  | Specifies the precision for the rounding (optional). |

## Return values

The `floor` function returns the rounded-down value of the specified decimal:

- If only *`x`* is provided, it returns an integer.
- If *`prec`* is provided, it returns a decimal with the specified precision.

## Examples

The following examples demonstrate the `floor` function:

1. Rounding down a decimal to an integer:

```lisp
(floor 3.5)
```
Result: `3`

2. Rounding down a decimal to a specified precision:

```lisp
(floor 100.15234 2)
```
Result: `100.15`

In these examples, `(floor 3.5)` rounds down the decimal `3.5` to `3`, and `(floor 100.15234 2)` rounds down the decimal `100.15234` to `100.15`. The `floor` function is useful for situations where you need to round down decimal values in Pact contracts.
