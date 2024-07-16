## power-of (^)

Use `^` to raises the `oper1` argument to the power of the `oper2` argument.

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

The following example demonstrates how to use the `^` function to raise `2` to the power of `3` in a Pact REPL: 

```pact
pact> (^ 2 3)
8
```
