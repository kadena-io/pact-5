## hyperlane-decode-token-message

Use `hyperlane-decode-token-message` to decode a base-64-unpadded encoded Hyperlane Token Message into an object containing recipient, amount, and chainId information.

### Basic syntax

To decode a Hyperlane Token Message using `hyperlane-decode-token-message`, use the following syntax:

```pact
(hyperlane-decode-token-message x)
```

### Arguments

| Name | Type | Description |
|------|------|-------------|
| `x`  | string | A base-64-unpadded encoded Hyperlane Token Message |

### Return value

The `hyperlane-decode-token-message` function returns an object with the following fields:

| Field | Type | Description |
|-------|------|-------------|
| `recipient` | GUARD | The recipient of the token message |
| `amount` | DECIMAL | The amount of tokens |
| `chainId` | STRING | The chain identifier |

### Examples

Here's an example of using `hyperlane-decode-token-message` to decode a Hyperlane Token Message:

```pact
pact> (hyperlane-decode-token-message "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAHsABHsicHJlZCI6ICJrZXlzLWFsbCIsICJrZXlzIjpbImRhMWEzMzliZDgyZDJjMmU5MTgwNjI2YTAwZGMwNDMyNzVkZWIzYWJhYmIyN2I1NzM4YWJmNmI5ZGNlZThkYjYiXX0")
{"amount": 0.000000000000000123,"chainId": "4","recipient": KeySet {keys: [da1a339bd82d2c2e9180626a00dc043275deb3ababb27b5738abf6b9dcee8db6],pred: keys-all}}
```

In this example, the function decodes the provided base-64-unpadded string and returns an object containing the decoded information:
- The `amount` is 0.000000000000000123
- The `chainId` is "4"
- The `recipient` is a KeySet with one key and a `keys-all` predicate
