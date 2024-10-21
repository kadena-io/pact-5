## select

Use `select` to retrieve full rows or specified columns from a table by applying a `where` clause to each row to determine whether to include the row or column in the selection.

### Basic syntax

To select full rows from a table based on a `where` clause, use the following syntax:

```pact
(select table where)
```

To select specific columns from a table based on a `where` clause, use the following syntax:

```pact
(select table columns where)
```

### Arguments

Use the following arguments to specify the table, columns, and `where` clause for selecting rows using the `select` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `table` | `table: <{row}>` | Specifies the table from which to select rows matching the where clause. |
| `columns` | [string] | Specifies the list of columns to select from the table matching the where clause (optional). |
| `where` | `row:object:<{row}>` | Specifies the `where` clause to apply to each row to determine inclusion. |

### Return value

The `select` function returns a list of objects representing the selected rows from the table that satisfy the `where` condition.

### Examples

The following example demonstrates how to use the `select` function  to select the columns `'firstName` and `'lastName` from the `people` table where the `name` is equal to "Fatima":

```pact
(select people ['firstName 'lastName] (where 'name (= "Fatima")))
```

The following example demonstrates how to select all columns from the `people` table where the `age` is less than 30:

```pact
(select people (where 'age (> 30)))
```
