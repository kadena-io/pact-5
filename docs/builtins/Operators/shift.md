## shift

Use `shift` to perform a bitwise shift operation on the integer `oper1` by `oper2` bits. 
If `oper2` is positive, this function shifts `oper1` to the left.
If `oper2` is negative, the function shifts `oper1` to the right. 
Right shifts perform sign extension on signed number types, filling the top bits with 1 if `oper1` is negative and with 0 otherwise.

### Basic syntax

To shift the integer `oper1` by `oper2` bits, use the following syntax:

```pact
(shift oper1 oper2)
```

### Arguments

Use the following arguments to specify the integer values to be shifted using the `shift` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `oper1` | integer | Specifies the integer value to be shifted. |
| `oper2` | integer | Specifies the number of bits to shift `oper1` by. |

### Return value

The `shift` function returns the result of shifting `oper1` by `oper2` bits.

### Examples

The following example demonstrates how to use the `shift` function to shift the integer 255 to the left by 8 bits:

```pact
(shift 255 8)
65280
```

The following example demonstrates how to use the `shift` function to shift the integer 255 to the right by 1 bit:

```pact
(shift 255 -1)
127
```

The following example demonstrates how to use the `shift` function to shift the negative integer -255 to the left by 8 bits:

```pact
(shift -255 8)
-65280
```

The following example demonstrates how to use the `shift` function to shift the negative integer -255 to the right by 1 bit:

```pact
(shift -255 -1)
-128
```

These examples illustrate how to use the `shift` function to perform bitwise shift operations on integers in Pact, either to the left or to the right, with sign extension for right shifts on signed numbers.
