Use `ln` to compute the natural logarithm of X.

## Basic syntax

To compute the natural logarithm of a value, use the following syntax:

ln *x*

## Argument

Use the following argument to specify the value for which you want to compute the natural logarithm using the `ln` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| x | integer, decimal | Specifies the value for which you want to compute the natural logarithm. |

## Return value

The `ln` function returns the natural logarithm of the specified value.

## Examples

The following example demonstrates the use of `ln` in the Pact REPL:

```lisp
pact>(round (ln 60) 6)
4.094345
```

In this example, the natural logarithm of 60 is computed and rounded to 6 decimal places, resulting in approximately 4.094345.
