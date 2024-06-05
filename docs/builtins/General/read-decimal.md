## read-decimal

Use `read-decimal` to parse a `key` string or number value from the top level of the message data body as a decimal.

### Basic syntax

To parse a `key` string as a decimal, use the following syntax:

```pact
(read-decimal key)
```

### Arguments

Use the following argument to specify the `key` to be parsed as a decimal using the `read-decimal` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `key` | string | Specifies the key string or number value to parse as a decimal. |

### Return value

The `read-decimal` function returns the parsed value as a decimal.

### Example

The following example demonstrates how to use the `read-decimal` function in a Pact script. 
This example parses the value specified for the `amount` key in the body of a message as a decimal and transfers it from one account to another:

```pact
(defun exec ()
   (transfer (read-msg "from") (read-msg "to") (read-decimal "amount")))
```

You can also use `read-decimal` to read values from JSON object payloads.
