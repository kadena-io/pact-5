## shift
The `shift` function performs a bitwise shift operation on the integer `X` by `Y` bits. If `Y` is positive, it shifts `X` to the left; otherwise, it shifts `X` to the right. Right shifts perform sign extension on signed number types, filling the top bits with 1 if `X` is negative and with 0 otherwise.

### Basic syntax

To shift the integer `X` by `Y` bits, use the following syntax:

`(shift X Y)`

### Arguments

Use the following arguments to specify the integer values to be shifted using the `shift` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `X` | `integer` | Specifies the integer value to be shifted. |
| `Y` | `integer` | Specifies the number of bits to shift `X` by. |

### Return value

The `shift` function returns the result of shifting `X` by `Y` bits.

### Examples

The following examples demonstrate the usage of the `shift` function within a Pact script.

To shift the integer 255 8 bits to the left:

```pact
(shift 255 8)
65280
```

To shift the integer 255 1 bit to the right:

```pact
(shift 255 -1)
127
```

To shift the negative integer -255 8 bits to the left:

```pact
(shift -255 8)
-65280
```

To shift the negative integer -255 1 bit to the right:

```pact
(shift -255 -1)
-128
```

These examples illustrate how to use the `shift` function to perform bitwise shift operations on integers in Pact, either to the left or to the right, with sign extension for right shifts on signed numbers.
