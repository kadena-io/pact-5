The `write` function writes an entry in the specified table for a given key with the data provided in the object column.

## Basic syntax

To write an entry in a table for a specific key with the provided object column data, use the following syntax:

write *table* *key* *object*

## Arguments

Use the following arguments to specify the table, key, and object data for writing using the `write` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| table | table:<{row}> | Specifies the table in which to write the entry. |
| key | string | Specifies the key for the entry to be written. |
| object | object:<{row}> | Specifies the object column data to be written for the key. |

## Return value

The `write` function returns a string indicating the success of the write operation.

## Examples

The following example demonstrates the usage of the `write` function within a Pact script. It writes an entry in the `accounts` table for the specified key with the provided object column data:

```lisp
(write accounts id { "balance": 100.0 })
```

This example illustrates how to use the `write` function to insert data into a table in Pact, enabling the storage of structured information for later retrieval and manipulation.
