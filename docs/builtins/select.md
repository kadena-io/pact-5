## select
The `select` function retrieves full rows or specified columns from a table by applying a `WHERE` clause to each row to determine inclusion.

### Basic syntax

To select full rows from a table based on a `WHERE` clause, use the following syntax:

select *table* *where*

To select specific columns from a table based on a `WHERE` clause, use the following syntax:

select *table* [*columns*] *where*

### Arguments

Use the following arguments to specify the table, columns, and `WHERE` clause for selecting rows using the `select` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| table | table:<{row}> | Specifies the table from which to select rows. |
| columns | [string] | (Optional) Specifies the list of columns to select from the table. |
| where | row:object:<{row}> | Specifies the `WHERE` clause to apply to each row to determine inclusion. |

### Return value

The `select` function returns a list of objects representing the selected rows from the table that satisfy the `WHERE` condition.

### Examples

The following examples demonstrate the usage of the `select` function within a Pact script.

To select the columns `'firstName` and `'lastName` from the `people` table where the `name` is equal to "Fatima":

```lisp
(select people ['firstName 'lastName] (where 'name (= "Fatima")))
```

To select all columns from the `people` table where the `age` is greater than 30:

```lisp
(select people (where 'age (> 30)))
```

These examples illustrate how to use the `select` function to retrieve rows or columns from a table based on specified conditions using a `WHERE` clause in Pact.
