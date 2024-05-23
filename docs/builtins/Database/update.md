## update
The `update` function writes an entry in the specified table for a given key with the data provided in the object column. It fails if data does not exist for the specified key.

### Basic syntax

To update an entry in `TABLE` for a specific `KEY` with the provided `OBJECT` column data, use the following syntax:

`(update table key object)`

### Arguments

Use the following arguments to specify the table, key, and object data for updating using the `update` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `TABLE` | `table:<{row}>` | Specifies the table in which to update the entry. |
| `KEY` | `string` | Specifies the key for the entry to be updated. |
| `OBJECT` | `object:<{row}>` | Specifies the object column data to be written for the key. |

### Return value

The `update` function returns a string indicating the success of the update operation.

### Examples

The following example demonstrates the usage of the `update` function within a Pact script. It updates an entry in the `accounts` table for the specified key with the provided object column data:

```lisp
(update accounts id { "balance": (+ bal amount), "change": amount, "note": "credit" })
```

This example illustrates how to use the `update` function to modify an entry in a table with new data in Pact, ensuring that the operation fails if data does not exist for the specified key.
