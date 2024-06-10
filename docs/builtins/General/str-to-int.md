## str-to-int

Use `str-to-int` to compute the integer value of the string `str` interpreted in base 10, or in a specified `base` ,if provided. 
The `str` argument can be up to 512 characters. 
If you specify the `base` argument, it must be between 2 and 16, or 64 to perform unpadded base64url conversion. 
Each digit in the `str` argument must be in the correct range for the base.

### Basic syntax

To compute the integer value of a string in base 10, use the following syntax:

```pact
(str-to-int str)
```

To compute the integer value of a string in a specified base, use the following syntax:

```pact
(str-to-int base str)
```

### Arguments

Use the following arguments to specify the string value and, optionally, the base for conversion using the `str-to-int` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `str` | string | Specifies the string value to be converted to an integer. |
| `base` | integer | (Optional) Specifies the base in which to interpret the string value. Must be between 2 and 16, or 64 for base64url encoding without padding. |

### Return value

The `str-to-int` function returns the `integer` value computed from the input string `str`.

### Examples

The following examples demonstrate how to use the `str-to-int` function to compute the integer value of a string in the Pact REPL.

To compute the integer value of the string "123456" interpreted in base 10:

```pact
pact> (str-to-int "123456")
123456
```

To compute the integer value of the string "abcdef123456" interpreted in base 16:

```pact
pact> (str-to-int 16 "abcdef123456")
188900967593046
```

To compute the integer value of the string "spirekey" interpreted in base 64 for base64url conversion without padding:

```pact
pact> (str-to-int 64 "spirekey")
196368781690802
```
