## env-chain-data

Use `env-chain-data` to define chain information for transactions in your testing environment.
You can this function to create an object with one or more of the following fields:

- `chain-id`: The chain identifier (0-19) for the blockchain where the transaction is executed.
- `block-height`: The height of the block that includes the transaction.
- `block-time`: The timestamp of the block that includes the transaction.
- `prev-block-hash`: The hash of the previous block.
- `sender`: The sender of the transaction.
- `gas-limit`: The gas limit for the transaction.
- `gas-price`: The gas price for the transaction.
- `gas-fee`: The gas fee for the transaction.

### Basic syntax

To set one or more chain data fields, use the following syntax:

```pact
(env-chain-data {object})
```

### Arguments

Use the following argument to specify the chain data you want to set using the `env-chain-data` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `object` | object | Specifies the chain data you want to be set for the transaction. |

### Return value

The `env-chain-data` function returns an object with the chain data you specify.

### Examples

The following example demonstrates how to use the `env-chain-data` function to set a chain identifier and block time for a transaction:

```pact
(begin-tx "create-token")
  (env-chain-data {"chain-id":"3","block-time": (time "2023-07-20T11:26:35Z")})
(commit-tx)
```
