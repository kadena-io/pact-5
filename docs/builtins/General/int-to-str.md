## int-to-str

Use `int-to-str` to represent an integer `value` as a string in a specified `base`.
The base can be any integer from 2 to 16, or 64 for unpadded base64URL representation.
Only positive values are allowed for base64URL conversion.

### Basic syntax

To represent an integer `value` as a string in a specified `base`, use the following syntax:

```pact
(int-to-str base value)
```

### Arguments

Use the following arguments to specify the base and integer value you want to convert using the `int-to-str` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `base` | integer | Specifies the base in which the integer value will be represented as a string. It can be any integer from 2 to 16, or 64 for unpadded base64-encoded representation. |
| `val` | integer | Specifies the integer value to be converted into a string representation. |

### Return value

The `int-to-str` function returns the string representation of the integer value in the specified base.

### Examples

The following example demonstrates how to use the `int-to-str` function in the Pact REPL:

```pact
pact> (int-to-str 16 65535)
"ffff"
```

In this example, the integer value 65535 is represented as a string in base 16, resulting in "ffff".

The following example illustrates converting the integer value 43981 into an unpadded base64-encoded string:

```pact
pact> (int-to-str 64 43981)
"q80"
```