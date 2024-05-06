## read-msg
The `read-msg` function is used to read a `KEY` from the top level of the message data body, or the data body itself if
`KEY` is not provided. It coerces the value to its corresponding Pact type, such as `string`, `integer`, `boolean`, `list`, or `object`.

### Basic syntax

To read a `KEY` from the message data body or the data body itself, use the following syntax:

`(read-msg KEY)`
`(read-msg)`

### Arguments

Use the following argument to specify the `key` to be read from the top level of the message data body using the `read-msg` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `KEY` | `string` | (Optional) Specifies the key to be read from the message data body. If not provided, reads the entire data body. |

### Return value

The `read-msg` function returns the value corresponding to the specified `KEY` from the message data body, or the entire data body if no `KEY` is provided. The value is coerced to its corresponding Pact type.

### Example

The following example demonstrates the usage of the `read-msg` function within a Pact script. It reads the `from` and `to` keys from the message data body and the `amount` as a decimal:

```lisp
(defun exec ()
   (transfer (read-msg "from") (read-msg "to") (read-decimal "amount")))
```

This example illustrates how to use the `read-msg` function to extract specific values from the message data body, coercing them to their corresponding types for further processing within a Pact script.
