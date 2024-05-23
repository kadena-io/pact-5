## int-to-str

Use `int-to-str` to represent an integer `value` as a string in a specified `base`.
The base can be any integer from 2 to 16, or 64 for unpadded base64URL representation.
Only positive values are allowed for base64URL conversion.

### Basic syntax

To represent an integer `value` as a string in a specified `base`, use the following syntax:

`(int-to-str base val)`

### Arguments

Use the following arguments to specify the base and integer value you want to convert using the `int-to-str` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `base` | `integer` | Specifies the base in which the integer value will be represented as a string. It can be any integer from 2 to 16, or 64 for unpadded base64URL representation. |
| `val` | `integer` | Specifies the integer value to be converted into a string representation. |

### Return value

The `int-to-str` function returns the string representation of the integer value in the specified base.

### Examples

The following examples demonstrate the use of `int-to-str` in the Pact REPL:

```lisp
pact>(int-to-str 16 65535)
"FFFF"
```

In this example, the integer value 65535 is represented as a string in base 16, resulting in "FFFF".

```lisp
pact>(int-to-str 64 43981)
"XA"
```

In this example, the integer value 43981 is represented as a string in base 64 (unpadded base64URL), resulting in "XA".
