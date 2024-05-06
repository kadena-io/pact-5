## xor
The `xor` function computes the bitwise XOR (exclusive OR) operation between two `integer`s.

### Basic syntax

To compute the bitwise XOR operation between two integers, use the following syntax:

`(xor X Y)`

### Arguments

Use the following arguments to specify the integers for the bitwise XOR operation using the `xor` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `X` | `integer` | Specifies the first integer for the XOR operation. |
| `Y` | `integer` | Specifies the second integer for the XOR operation. |

### Return value

The `xor` function returns the result of the bitwise XOR operation as an integer.

### Examples

The following examples demonstrate the usage of the `xor` function within the Pact REPL. They compute the bitwise XOR operation between two integers:

```lisp
(xor 127 64)
63
```

```lisp
(xor 5 -7)
-4
```

These examples illustrate how to use the `xor` function to perform bitwise XOR operations on integers in Pact, facilitating bitwise manipulation and logic operations.
