## mod

Use `mod` to compute the remainder of `oper1` divided by `oper2`.

### Basic syntax

To compute the remainder of `oper1` divided by `oper2`, use the following syntax:

```
(mod oper1 oper2)
```

### Arguments

Use the following arguments to specify the integers for which you want to compute the remainder using the `mod` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `oper1` | integer | Specifies the dividend. |
| `oper2` | integer | Specifies the divisor. |

### Return value

The `mod` function returns the remainder of the division of `oper1` by `oper2`.

### Examples

The following example demonstrates how to use the `mod` function to compute the remainder when 13 is divided by 8:

```pact
pact>(mod 13 8)
5
```
