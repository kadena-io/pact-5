## env-hash

_hash_&nbsp;`string` _&rarr;_&nbsp;`string`

Set current transaction hash. HASH must be an unpadded base64-url encoded BLAKE2b 256-bit hash.

```bash
pact> (env-hash (hash "hello"))
"Set tx hash to Mk3PAn3UowqTLEQfNlol6GsXPe-kuOWJSCU0cbgbcs8"
```
