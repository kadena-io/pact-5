## select
The `select` function retrieves full rows or specified columns from a table by applying a `WHERE` clause to each row to determine inclusion.

### Basic syntax

To select full rows from a table based on a `WHERE` clause, use the following syntax:

`(select TABLE WHERE)`

To select specific columns from a table based on a `WHERE` clause, use the following syntax:

`(select TABLE COLUMNS WHERE)`

### Arguments

Use the following arguments to specify the table, columns, and `WHERE` clause for selecting rows using the `select` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `TABLE` | `table:<{row}>` | Specifies the table from which to select rows. |
| `COLUMNS` | `[string]` | (Optional) Specifies the list of columns to select from the table. |
| `WHERE` | `row:object:<{row}>` | Specifies the `WHERE` clause to apply to each row to determine inclusion. |

### Return value

The `select` function returns a list of objects representing the selected rows from the table that satisfy the `WHERE` condition.

### Examples

The following examples demonstrate the usage of the `select` function within a Pact script.

To select the columns `'firstName` and `'lastName` from the `people` table where the `name` is equal to "Fatima":

```pact
(select people ['firstName 'lastName] (where 'name (= "Fatima")))
```

To select all columns from the `people` table where the `age` is less than 30:

```pact
(select people (where 'age (> 30)))
```

These examples illustrate how to use the `select` function to retrieve rows or columns from a table based on specified conditions using a `WHERE` clause in Pact.
