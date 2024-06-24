## txlog

Use `txlog` to return all updates made to a specified table in a particular transaction identified by its transaction identifier.

### Basic syntax

To retrieve all updates made to the specified `table` in a specific transaction identifier, use the following syntax:

```pact
(txlog TABLE TXID)
```

### Arguments

Use the following arguments to specify the table and transaction identifier for retrieval using the `txlog` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `table` | table:<{row}> | Specifies the table from which to retrieve updates. |
| `txid` | integer | Specifies the transaction ID (TXID) for which updates are to be retrieved. |

### Return value

The `txlog` function returns a list of objects representing all updates made to the specified table in the transaction identified by the `txid` provided.

### Examples

The following example demonstrates how to use the `txlog` function to retrieve all updates made to the `accounts` table in the transaction with the `123485945` transaction identifier:

```pact
(txlog accounts 123485945)
```
