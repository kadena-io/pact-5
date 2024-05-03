The `shift` function performs a bitwise shift operation on the integer *`X`* by *`Y`* bits. If *`Y`* is positive, it shifts *`X`* to the left; otherwise, it shifts *`X`* to the right. Right shifts perform sign extension on signed number types, filling the top bits with 1 if *`X`* is negative and with 0 otherwise.

### Basic syntax

To shift the integer *`X`* by *`Y`* bits, use the following syntax:

shift *x* *y*

### Arguments

Use the following arguments to specify the integer values to be shifted using the `shift` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| x | integer | Specifies the integer value to be shifted. |
| y | integer | Specifies the number of bits to shift *`x`* by. |

### Return value

The `shift` function returns the result of shifting *`x`* by *`y`* bits.

### Examples

The following examples demonstrate the usage of the `shift` function within a Pact script.

To shift the integer 255 8 bits to the left:

```lisp
(shift 255 8)
```

To shift the integer 255 1 bit to the right:

```lisp
(shift 255 -1)
```

To shift the negative integer -255 8 bits to the left:

```lisp
(shift -255 8)
```

To shift the negative integer -255 1 bit to the right:

```lisp
(shift -255 -1)
```

These examples illustrate how to use the `shift` function to perform bitwise shift operations on integers in Pact, either to the left or to the right, with sign extension for right shifts on signed numbers.
