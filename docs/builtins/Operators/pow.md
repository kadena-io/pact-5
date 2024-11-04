## power-of (^)

Use the exponentiation operator `^` to raise the `oper1` argument to the power of the `oper2` argument.
You can use this operator with both integer or decimal values.
When the operands are integers, the result is an integer if the exponent is not negative.
When the operands are decimals or you use a negative exponent, the result is always a decimal.

### Basic syntax

To raise `oper1` to the power of `oper2`, use the following syntax:

```pact
(^ oper1 oper2)
```

### Arguments

Use the following arguments to specify the base and exponent for raising to a power using the `^` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `oper1` | integer or decimal | Specifies the base value. |
| `oper2` | integer or decimal | Specifies the exponent value. |

### Return value

The `^` function returns the result of raising `oper1` to the power of `oper2`.

### Examples

The following example demonstrates how to use the `^` function with integer values to raise `2` to the power of `3` in a Pact REPL: 

```pact
pact> (^ 2 3)
8
```

The following example demonstrates how to use the `^` function with decimal values to raise `5.5` to the power of `2.0` in a Pact REPL: 

```pact
pact> (^ 5.5 2.0)
30.25
```

The following example demonstrates using the `^` function with a negative exponent value: 

```pact
pact> (^ 2 -3)
0.125
```
