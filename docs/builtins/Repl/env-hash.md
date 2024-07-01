## env-hash

Use `env-hash` to set the current transaction hash.

### Basic syntax

```pact
(env-hash hash)
```

### Arguments

Use the following argument when using the `env-hash` Pact function.

| Argument | Type   | Description   |
|----------|--------|---------------|
| `hash`   | `string` | Specifies the hash value to set as the current transaction hash. The hash must be an unpadded base64-url encoded BLAKE2b 256-bit hash. |

### Return value

The `env-hash` function returns a string indicating that the transaction hash has been set to the specified value.

### Example

The following example demonstrates setting the transaction hash within a Pact REPL:

```pact
pact> (env-hash (hash "hello"))
"Set tx hash to Mk3PAn3UowqTLEQfNlol6GsXPe-kuOWJSCU0cbgbcs8"
```
