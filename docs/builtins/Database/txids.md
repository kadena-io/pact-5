## txids

Ue `txids` to return all of the transaction identifiers greater than or equal to a specified `txid` in a given table.

### Basic syntax

To retrieve all transaction identifier values greater than or equal to a specified `txid` in a table, use the following syntax:

```pact
(txids table txid)
```

### Arguments

Use the following arguments to specify the table and transaction identifier for retrieval using the `txids` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `table` | table:<{row}> | Specifies the table from which to retrieve transaction identifiers. |
| `txid` | integer | Specifies the transaction identifier value to compare against. |

### Return value

The `txids` function returns a list of transaction identifiers greater than or equal to the specified `txid` in the specified table.

### Examples

The following example demonstrates how to use the `txids` function to retrieve all transaction identifier values greater than or equal to `123849535` in the `accounts` table:

```pact
(txids accounts 123849535)
```
