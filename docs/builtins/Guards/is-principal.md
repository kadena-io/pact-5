## is-principal

Use `is-principal` to determine whether a principal string conforms to the principal format *without* proving its validity.

### Basic syntax

To check whether a `principal` string conforms to the principal format, use the following syntax:

`(is-principal principal)`

### Arguments

Use the following argument to specify the principal string you want to check using the `is-principal` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `principal` | `string` | Specifies the principal string to be checked. |

### Return value

The `is-principal` function returns a boolean value indicating whether the input principal string conforms to the principal format.

### Examples

The following example demonstrates the use of `is-principal` within an `enforce` statement:

```lisp
pact>(enforce (is-principal "k:462e97a099987f55f6a2b52e7bfd52a36b4b5b470fed0816a3d9b26f9450ba69") "Invalid account structure: non-principal account")
true
```

In this example, the `is-principal` function checks whether the provided principal string conforms to the principal format. If the string conforms to the format, the `enforce` statement proceeds without triggering an error; otherwise, it throws an error with the message "Invalid account structure: non-principal account".
