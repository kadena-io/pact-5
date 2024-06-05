## read-string

Use `read-string` to parse the specified `key` string or number value from the top level of the message data body as a `string`.

### Basic syntax

To parse a `key` string as a string, use the following syntax:

```pact
(read-string key)
```

### Arguments

Use the following argument to specify the `key` to be parsed as a string using the `read-string` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `key` | string | Specifies the key string or number value to parse as a string. |

### Return value

The `read-string` function returns the parsed value as a `string`.

### Example

The following example demonstrates how to use the `read-string` function in a Pact script. 
This example parses the `sender` from the data body of a message as a string:

```pact
(read-string 'sender)
```
