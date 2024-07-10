## hyperlane-encode-token-message

Use `hyperlane-encode-token-message` to encode an object into a Hyperlane Token message as an unpadded base64url string.

### Basic syntax

To encode an object into a Hyperlane Token message using `hyperlane-encode-token-message`, use the following syntax:

```pact
(hyperlane-encode-token-message x)
```

### Arguments

| Name | Type | Description |
|------|------|-------------|
| `x`  | object | An object containing `recipient`, `amount`, and `chainId` |

The object should have the following structure:
- `recipient`: string (base64 encoded representation of a guard)
- `amount`: decimal
- `chainId`: string

### Return value

The `hyperlane-encode-token-message` function returns a string representing the encoded Hyperlane Token message in unpadded base64url format.

### Examples

Here's an example of using `hyperlane-encode-token-message` to encode an object into a Hyperlane Token message:

```pact
pact> (hyperlane-encode-token-message {"recipient": "eyJwcmVkIjogImtleXMtYWxsIiwgImtleXMiOlsiZGExYTMzOWJkODJkMmMyZTkxODA2MjZhMDBkYzA0MzI3NWRlYjNhYmFiYjI3YjU3MzhhYmY2YjlkY2VlOGRiNiJdfQ", "amount":123000000000000000.0, "chainId":"4"})
"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAbT72StfgAAABHsicHJlZCI6ICJrZXlzLWFsbCIsICJrZXlzIjpbImRhMWEzMzliZDgyZDJjMmU5MTgwNjI2YTAwZGMwNDMyNzVkZWIzYWJhYmIyN2I1NzM4YWJmNmI5ZGNlZThkYjYiXX0"
```

In this example, the function encodes the provided object into a Hyperlane Token message. The object contains:
- A `recipient` represented as a base64 encoded string (which itself represents a guard)
- An `amount` of 123000000000000000.0
- A `chainId` of "4"

The function returns the encoded message as an unpadded base64url string.
