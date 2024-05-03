The `read-decimal` function is used to parse a *`key`* string or number value from the top level of the message data body as a decimal.

### Basic syntax

To parse a *`key`* string as a decimal, use the following syntax:

read-decimal *key*

### Arguments

Use the following argument to specify the *`key`* to be parsed as a decimal using the `read-decimal` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| key | string | Specifies the key string or number value to parse as a decimal. |

### Return value

The `read-decimal` function returns the parsed value as a decimal.

### Example

The following example demonstrates the usage of the `read-decimal` function within a Pact script. It parses the *`amount`* from the message data body as a decimal and transfers it from one account to another:

```lisp
(defun exec ()
   (transfer (read-msg "from") (read-msg "to") (read-decimal "amount")))
```

This example illustrates how to use the `read-decimal` function to parse a specific value as a decimal for further processing within a Pact script.
