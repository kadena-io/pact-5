## read

Use `read` to retrieve a row from a specified table by its key. You can optionally specify a subset of columns to return.

### Basic syntax

To read an entire row from a table, use the following syntax:

```pact
(read table key)
```

To read specific columns from a row in a table, use the following syntax:

```pact
(read table key columns)
```

### Arguments

Use the following arguments to specify the table, key, and optional columns when using the `read` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `table` | `table: <{row}>` | Specifies the table from which to read the row. In the table schema, a `row` represents the structure of each row in the table. |
| `key` | string | Specifies the key of the row to read from the table. |
| `columns` | [string] | Specifies one or more column names to return from the row. If not provided, the entire row is returned (optional). |

### Return value

The `read` function returns an object representing the requested row or columns from the specified table.

- If you don't specify one or more `columns` as parameters, the function returns an object with the structure of the entire row.
- If you specify one or more `columns` as parameters, the function returns an object containing only the specified columns from the row.

### Examples

he following example demonstrates defining a table schema for an `accounts` table that stores information about bank accounts, including the balance, currency, and owner of each account:

```pact
(defschema account
  balance:decimal
  currency:string
  owner:string)

(deftable accounts:{account})
```

You can then use the `read` function to read a row from the `accounts` table by its key `id` and retrieve only the `balance` and `currency` columns:

```pact
(read accounts id ['balance 'currency])
```
