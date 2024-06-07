## read

Use `read` to retrieve a row from a specified table by its key. You can optionally specify a subset of columns to return.

### Basic syntax

To read an entire row from a table, use the following syntax:

`(read table key)`

To read specific columns from a row in a table, use the following syntax:

`(read table key columns)`

### Arguments

Use the following arguments to specify the table, key, and optional columns when using the `read` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `table` | `table` | Specifies the table from which to read the row. The table schema is `table:<{row}>`, where `row` represents the structure of each row in the table. |
| `key` | `string` | Specifies the key of the row to read from the table. |
| `columns` | `[string]` | (Optional) Specifies an array of column names to return from the row. If not provided, the entire row is returned. |

### Return values

The `read` function returns an object representing the requested row or columns from the specified table.

- If `columns` is not provided, the function returns an object with the schema `object:<{row}>`, where `row` represents the structure of the entire row.
- If `columns` is provided, the function returns an object containing only the specified columns from the row.

### Examples

Suppose we have a table named `accounts` with the following schema:

```lisp
(defschema account
  balance:decimal
  ccy:string
  owner:string)

(deftable accounts:{account})
```

The `accounts` table stores information about bank accounts, including the balance, currency (ccy), and owner of each account.

The following example reads a row from the `accounts` table by its key `id` and retrieves only the `balance` and `ccy` columns:

```pact
(read accounts id ['balance 'ccy])
```