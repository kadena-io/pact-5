The `read-string` function is used to parse a *`key`* string or number value from the top level of the message data body as a string.

### Basic syntax

To parse a *`key`* string as a string, use the following syntax:

read-string *key*

### Arguments

Use the following argument to specify the *`key`* to be parsed as a string using the `read-string` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| key | string | Specifies the key string or number value to parse as a string. |

### Return value

The `read-string` function returns the parsed value as a string.

### Example

The following example demonstrates the usage of the `read-string` function within a Pact script. It parses the *`sender`* from the message data body as a string:

```lisp
(read-string "sender")
```

This example illustrates how to use the `read-string` function to parse a specific value as a string for further processing within a Pact script.
