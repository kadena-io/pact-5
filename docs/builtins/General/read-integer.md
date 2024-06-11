## read-integer

Use `read-integer` to parse a `key` string or number value from the top level of the message data body as an integer.

### Basic syntax

To parse a `key` string as an integer, use the following syntax:

```pact
(read-integer key)
```

### Arguments

Use the following argument to specify the `key` to be parsed as an integer using the `read-integer` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `key` | string | Specifies the key string or number value to parse as an integer. |

### Return value

The `read-integer` function returns the parsed value as an integer.

### Example

The following example demonstrates how to use the `read-integer` function in a Pact script. 
This example parses the value specified for the `age` key in the body of a message as an integer:

```pact
(read-integer 'age)
```
