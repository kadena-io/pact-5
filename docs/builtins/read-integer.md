## read-integer
The `read-integer` function is used to parse a `key` string or number value from the top level of the message data body as an integer.

### Basic syntax

To parse a `key` string as an integer, use the following syntax:

`(read-integer key)`

### Arguments

Use the following argument to specify the `key` to be parsed as an integer using the `read-integer` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `key` | `string` | Specifies the key string or number value to parse as an integer. |

### Return value

The `read-integer` function returns the parsed value as an integer.

### Example

The following example demonstrates the usage of the `read-integer` function within a Pact script. It parses the `age` from the message data body as an integer:

```lisp
(read-integer 'age)
```

This example illustrates how to use the `read-integer` function to parse a specific value as an integer for further processing within a Pact script.
