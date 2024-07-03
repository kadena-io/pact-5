## bitwise-and (&)

Use `&` to compute the bitwise AND operation between the first integer `oper1` value and the second integer `oper2` value.

### Basic syntax

To compute the bitwise AND operation between `oper1` and `oper2`, use the following syntax:

```pact
(& oper1 oper2)
```

### Arguments

Use the following arguments to specify the values for bitwise AND operation using the `&` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `oper1` | integer | Specifies the first operand. |
| `oper2` | integer | Specifies the second operand. |

### Return value

The `&` function returns the result of the bitwise AND operation between `oper1` and `oper2`.

### Examples

The following examples demonstrate how to use the `&` function to perform bitwise AND manipulation of integer values in a Pact REPL:

```pact
pact> (& 2 3)
2

pact> (& 5 -7)
1
```
