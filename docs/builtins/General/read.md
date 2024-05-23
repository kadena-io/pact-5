## read
Use the `read` function to retrieve information from a specified `table` based on a given `key`. This function returns the database record object corresponding to the `key`. Optionally, you can specify a list of `columns` to retrieve specific data from the record.

### Basic syntax

To retrieve the entire record associated with the specified `key`, use the following syntax:

`(read table key)`

To retrieve specific columns from the record associated with the specified `key`, use the following syntax:

`(read table key [columns])`

### Arguments

Use the following arguments to specify the table and key for retrieving data using the `read` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `table` | `table:<{row}>` | Specifies the table from which to retrieve data. |
| `key` | `string` | Specifies the unique identifier (key) for the record to retrieve. |

### Options

Use the following option to specify particular columns to retrieve from the record associated with the specified `key` in the `read` Pact function.

| Option | Type | Description |
| --- | --- | --- |
| `columns` | `[string]` | Specifies the list of columns to retrieve from the record. |

### Return values

The `read` function returns the database record object corresponding to the specified `key`, or the specified columns from the record, as an object.

### Examples

The following example retrieves the entire record associated with the `user_1` key from the `accounts` table:

```lisp
pact>(read accounts "user_1")
```

The following example retrieves the `balance` and `ccy` columns from the record associated with the `user_1` key in the `accounts` table:

```lisp
pact>(read accounts "user_1" ['balance 'ccy])
```

These examples illustrate how to use the `read` function to fetch data from a Pact table.
