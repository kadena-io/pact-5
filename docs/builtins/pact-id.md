## pact-id

Use `pact-id` to return the ID if called during the current pact execution, failing if not.

### Basic syntax

To return the ID during the current pact execution, use the following syntax:

`(pact-id)`

### Return value

The `pact-id` function returns a string representing the ID of the current pact execution.

### Examples

The `pact-id` function is called without any arguments. It returns the ID if called during the current pact execution.

```lisp
(pact-id)
```

In this example, `pact-id` is called to retrieve the ID of the current pact execution.
