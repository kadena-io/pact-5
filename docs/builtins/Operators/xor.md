## xor

Use `xor` to compute the bitwise exclusive OR (xor) operation between two integer arguments.

### Basic syntax

To compute the bitwise XOR operation between two integers, use the following syntax:

```pact
(xor oper1 oper2)
```

### Arguments

Use the following arguments to specify the integers for the bitwise XOR operation using the `xor` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `oper1` | integer | Specifies the first integer for the XOR operation. |
| `oper2` | integer | Specifies the second integer for the XOR operation. |

### Return value

The `xor` function returns the result of the bitwise XOR operation as an integer.

### Examples

The following examples demonstrate how to use the `xor` function to compute the bitwise XOR operation between two integers:

```pact
(xor 127 64)
63

(xor 5 -7)
-4
```
