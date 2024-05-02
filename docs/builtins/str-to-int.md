The `str-to-int` function computes the integer value of the string *`STR-VAL`* interpreted in base 10, or in a specified *`BASE`* if provided. The length of *`STR-VAL`* can be up to 512 characters. If *`BASE`* is specified, it must be between 2 and 16, or 64 to perform unpadded base64url conversion. Each digit in *`STR-VAL`* must be in the correct range for the base.

## Basic syntax

To compute the integer value of a string in base 10, use the following syntax:

str-to-int *str-val*

To compute the integer value of a string in a specified base, use the following syntax:

str-to-int *base* *str-val*

## Arguments

Use the following arguments to specify the string value and, optionally, the base for conversion using the `str-to-int` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| str-val | string | Specifies the string value to be converted to an integer. |
| base | integer | (Optional) Specifies the base in which to interpret the string value. Must be between 2 and 16, or 64 for unpadded base64url conversion. |

## Return value

The `str-to-int` function returns the integer value computed from the input string *`STR-VAL`*.

## Examples

The following examples demonstrate the usage of the `str-to-int` function within a Pact script.

To compute the integer value of the string "abcdef123456" interpreted in base 16:

```lisp
(str-to-int 16 "abcdef123456")
```

To compute the integer value of the string "123456" interpreted in base 10:

```lisp
(str-to-int "123456")
```

To compute the integer value of the string "q80" interpreted in base 64 for unpadded base64url conversion:

```lisp
(str-to-int 64 "q80")
```

These examples illustrate how to use the `str-to-int` function to compute the integer value of a string in base 10 or in a specified base in Pact.
