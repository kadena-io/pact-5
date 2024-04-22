Use `chain-data` to retrieve transaction public metadata, returning an object with fields including 'chain-id', 'block-height', 'block-time', 'prev-block-hash', 'sender', 'gas-limit', 'gas-price', and 'gas-fee'.

## Basic syntax

To retrieve transaction public metadata using `chain-data`, use the following syntax:

chain-data

## Return values

The `chain-data` function returns an object with the following fields of public transaction metadata:

- 'chain-id': The ID of the blockchain.
- 'block-height': The height of the block.
- 'block-time': The timestamp of the block.
- 'prev-block-hash': The hash of the previous block.
- 'sender': The sender of the transaction.
- 'gas-limit': The gas limit for the transaction.
- 'gas-price': The gas price for the transaction.
- 'gas-fee': The gas fee for the transaction.

## Examples

The following example retrieves transaction public metadata in the Pact REPL:

```lisp
pact>(chain-data)
{
  "chain-id": "example-chain",
  "block-height": 12345,
  "block-time": "2024-04-22T12:00:00Z",
  "prev-block-hash": "0xabc123...",
  "sender": "sender-address",
  "gas-limit": 10000,
  "gas-price": 0.00001,
  "gas-fee": 0.1
}
```

In this example, `chain-data` returns an object with the specified fields containing the public metadata of the transaction.
