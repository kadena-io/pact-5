## write

Use `write` to write the specified `{column:value}` object in the specified `table` for a given `key` row.
The `write` function is similar to the `insert` and `update` functions, except that:

- The `insert` function fails if data already exists for the specified `key` row.
- The `update` function fails if data does not exist for the specified `key` row.

In both of these cases, the `write` function can succeed and update the table without failing.

### Basic syntax

To write an entry in a specified `table` for a specific `key` with the provided `object`, use the following syntax:

```pact
(write table key object)
```

### Arguments

Use the following arguments to specify the table, key, and object data for writing using the `write` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `table` | `table: <{row}>` | Specifies the table in which to write the entry. |
| `key` | string | Specifies the key for the entry to be written. |
| `object` | object | Specifies the object column data to be written for the key. |

### Return value

The `write` function returns a `string` indicating the success of the write operation.

### Examples

The following example demonstrates how to use the `write` function to write an entry in the `accounts` table for the specified key with the provided object column data:

```pact
(write accounts id { "balance": 100.0 })
```

This example illustrates inserting data into the balance column in the `accounts` table using the `id` as the row key. 
