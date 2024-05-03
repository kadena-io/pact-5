## txids
The `txids` function returns all transaction IDs (txids) greater than or equal to a specified txid in a given table.

### Basic syntax

To retrieve all txid values greater than or equal to a specified txid in a table, use the following syntax:

txids *table* *txid*

### Arguments

Use the following arguments to specify the table and txid for retrieval using the `txids` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| table | table:<{row}> | Specifies the table from which to retrieve txids. |
| txid | integer | Specifies the txid value to compare against. |

### Return value

The `txids` function returns a list of transaction IDs (txids) greater than or equal to the specified txid in the table.

### Examples

The following example demonstrates the usage of the `txids` function within a Pact script. It retrieves all txid values greater than or equal to 123849535 in the `accounts` table:

```lisp
(txids accounts 123849535)
```

This example illustrates how to use the `txids` function to retrieve transaction IDs (txids) from a table in Pact based on a specified txid.
