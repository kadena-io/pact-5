## divide (/)

Use `/` to divide the first argument `oper1` by the second argument `oper2`.
Note that you can use this function to divide interger values or decimal values.
However, you should use the same type for both `oper1` and `oper2` values.

### Basic syntax

To divide `oper1` by `oper2`, use the following syntax:

```pact
(/ oper1 oper2)
```

### Arguments

Use the following arguments to specify the values for division using the `/` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `oper1` | integer or decimal | Specifies the value of the dividend. |
| `oper2` | integer or decimal | Specifies the divisor. |

### Return value

The `/` function returns the result of dividing `oper1` by `oper2`.

### Examples

The following examples demonstrate how to use the `/` function to divide two values in a Pact REPL:

```pact
pact> (/ 10.0 2.0)
5.0

pact> (/ 8 3)
2
```
