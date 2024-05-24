## try
The `try` function attempts a pure action, returning a default value in the case of failure. Pure expressions are expressions that do not involve I/O operations or work with non-deterministic state, unlike impure expressions such as reading and writing to a table.

### Basic syntax

To attempt a pure action and return a default value in case of failure, use the following syntax:

`(try DEFAULT ACTION)`

### Arguments

Use the following arguments to specify the default value and the action to be attempted using the `try` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `DEFAULT` | `<a>` | Specifies the default value to be returned in case the action fails. |
| `ACTION` | `<a>` | Specifies the action to be attempted. |

### Return value

The `try` function returns the result of the attempted action. If the action fails, it returns the specified default value.

### Examples

The following example demonstrates the usage of the `try` function within the Pact REPL. It attempts to enforce a condition, and if it fails, it returns the default value:

```pact
pact>(try 3 (enforce (= 1 2) "this will definitely fail"))
3
```

This example illustrates how to use the `try` function to attempt a pure action and return a default value in case of failure in Pact.
