Use `create-table` to create a table identified by the specified *`TABLE`*.

### Basic syntax

To create a table identified by *`TABLE`*, use the following syntax:

create-table *`TABLE`*

### Arguments

Use the following argument to specify the *`TABLE`* for the `create-table` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| table | table:<{row}> | Specifies the table to create. |

### Return values

The `create-table` function returns a string representing the identifier of the created *`TABLE`*.

### Example

The following example demonstrates the `create-table` function:

```lisp
(create-table accounts)
```

In this example, `(create-table accounts)` is used to create a table identified by `accounts`. This creates a new table in Pact that can be used for storing data, and the function returns a string representing the identifier of the created table.
