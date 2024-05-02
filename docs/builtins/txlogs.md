The `txlog` function returns all updates made to a specified table in a particular transaction identified by its transaction ID (TXID).

## Basic syntax

To retrieve all updates made to a table in a specific transaction, use the following syntax:

txlog *table* *txid*

## Arguments

Use the following arguments to specify the table and transaction ID (TXID) for retrieval using the `txlog` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| table | table:<{row}> | Specifies the table from which to retrieve updates. |
| txid | integer | Specifies the transaction ID (TXID) for which updates are to be retrieved. |

## Return value

The `txlog` function returns a list of objects representing all updates made to the specified table in the transaction identified by the provided TXID.

## Examples

The following example demonstrates the usage of the `txlog` function within a Pact script. It retrieves all updates made to the `accounts` table in the transaction with the TXID 123485945:

```lisp
(txlog accounts 123485945)
```

This example illustrates how to use the `txlog` function to obtain the updates performed on a specific table in a given transaction in Pact.
