## chain-data

Use `chain-data` to retrieve the blockchain-specific public metadata for a transaction. 
This function returns an object with the following fields:

- `chain-id`: The chain identifier (0-19) for the blockchain where the transaction was executed.
- `block-height`: The height of the block that includes the transaction.
- `block-time`: The timestamp of the block that includes the transaction.
- `prev-block-hash`: The hash of the previous block.
- `sender`: The sender of the transaction.
- `gas-limit`: The gas limit for the transaction.
- `gas-price`: The gas price for the transaction.
- `gas-fee`: The gas fee for the transaction.

### Basic syntax

To retrieve the public metadata for a transaction using `chain-data`, use the following syntax:

```pact
(chain-data)
```

### Arguments

You can use the `chain-data` function without arguments in code that identifies the transaction that you want to return metadata for.

### Return value

The `chain-data` function returns the public metadata for a transaction as an object with the following fields

| Field | Type | Description
| ----- | ---- | -----------
| `chain-id` | string | The chain identifier (0-19) for the blockchain where the transaction was executed.
| `block-height` | integer | The height of the block that includes the transaction.
| `block-time` | time | The timestamp of the block that includes the transaction.
| `prev-block-hash` | string | The hash of the previous block.
| `sender` | string | The sender of the transaction.
| `gas-limit` | integer | The gas limit for the transaction.
| `gas-price` | decimal | The gas price for the transaction.
| `gas-fee` | decimal | The gas fee for the transaction.

### Examples

If you call the `chain-data` function in the Pact REPL without providing a transaction context in the surrounding code, the function returns the object with placeholder fields.
For example:

```pact
{"block-height": 0
,"block-time": "1970-01-01T00:00:00Z"
,"chain-id": ""
,"gas-limit": 0
,"gas-price": 0.0
,"prev-block-hash": ""
,"sender": ""}
```

If you provide context for the call, the function returns an object with fields similar to the following:

```pact
pact> (chain-data)
{
  "chain-id": "3",
  "block-height": 4357306,
  "block-time": "2024-06-06 20:12:56 UTC",
  "prev-block-hash": "33caae279bd584b655283b7d692d7e7b408d6549869c5eb6dcf2dc60021c3916",
  "sender": "k:1d5a5e10eb15355422ad66b6c12167bdbb23b1e1ef674ea032175d220b242ed4,
  "gas-limit": 2320,
  "gas-price": 1.9981e-7,
  "gas-fee": 726
}
```

In most cases, you use `chain-data` in Pact modules or in combination with frontend libraries to return information in the context of a specific transaction.
The following example illustrates using chain-data in a Pact module to get the block time from a transaction:

```pact
(let ((curr-time:time (at 'block-time (chain-data))))
```
