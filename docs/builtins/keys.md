## keys

Use `keys` to return all keys present in a specified table.

### Basic syntax

To retrieve all keys present in a `table`, use the following syntax:

`(keys table)`

### Arguments

Use the following argument to specify the table from which you want to retrieve keys using the `keys` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `table` | table<{row}> | Specifies the table from which keys will be retrieved. |

### Return value

The `keys` function returns a list of strings containing all keys present in the specified table.

### Examples

The following example demonstrates the use of `keys` in the Pact REPL to retrieve all keys present in the "accounts" table:

```lisp
(keys accounts)
```

In this example, all keys present in the "accounts" table are returned as a list of strings.
