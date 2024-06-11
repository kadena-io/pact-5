## read-msg

Use `read-msg` to read a specific `key` from the top level of the message data body, or to read the data body itself if a `key` is not provided. 
In reading the value, this function enforces its corresponding Pact type, such as `string`, `integer`, `boolean`, `list`, or `object`.

### Basic syntax

To read the value for a specified `key` from the body of a message, use the following syntax:

```pact
(read-msg key)
```

To read the complete message data body, use the following syntax:

```pact
(read-msg)
```

### Arguments

Use the following argument to specify the `key` to be read from the top level of the message data body using the `read-msg` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `key` | string | (Optional) Specifies the key to be read from the message data body. If you don't provide this argument, the function reads the entire data body. |

### Return value

The `read-msg` function returns the value corresponding to the specified `key` from the message data body, or the entire data body if no `key` is provided. 
The data type for the return is its corresponding Pact type.

### Example

The following example demonstrates how to use the `read-msg` function in a Pact script. 
This example reads the `from` and `to` keys from the data body of a message as strings and the `amount` as a decimal:

```pact
(defun exec ()
   (transfer (read-msg "from") (read-msg "to") (read-decimal "amount")))
```

This example illustrates how to use the `read-msg` function to extract specific values from the body of a message for further processing in a Pact script.
