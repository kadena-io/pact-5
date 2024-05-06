## typeof
The `typeof` function returns the type of the provided value `X` as a string.

### Basic syntax

To determine the type of a value, use the following syntax:

`(typeof X)`

### Argument

Use the following argument to specify the value for which to determine the type using the `typeof` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `X` | `<a>` | Specifies the value for which to determine the type. |

### Return value

The `typeof` function returns the type of the provided value `X` as a string.

### Examples

The following example demonstrates the usage of the `typeof` function within a Pact script. It determines the type of the string `"hello"`:

```lisp
(typeof "hello")
"string"
```

This example illustrates how to use the `typeof` function to determine the type of a value in Pact.
