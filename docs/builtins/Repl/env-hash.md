## env-hash

Use `env-hash` to set the current transaction hash.

### Basic syntax

To set the current transaction hash, use the following syntax:

```pact
(env-hash hash)
```

### Arguments

Use the following argument when using the `env-hash` Pact function.

| Argument | Type   | Description   |
|----------|--------|---------------|
| `hash` | string | Specifies the hash value to set as the current transaction hash. The hash must be an unpadded base64-url encoded BLAKE2b 256-bit hash. |

## Return value

The `env-hash` function returns a string indicating that the transaction hash has been set to the specified value.

## Example

The following example demonstrates how to use the `env-hash` function to set the transaction hash using the base64-url encoded string for the `hello` string:

```pact
pact> (env-hash (hash "hello"))
"Set tx hash to Mk3PAn3UowqTLEQfNlol6GsXPe-kuOWJSCU0cbgbcs8"
```

The following example illustrates using the `env-hash` function to create a transaction hash for the test transaction that creates a token:

```pact
(env-hash (hash "create-tokens"))
```
