## env-sigs

Use `env-sigs` to set transaction signature keys and their associated capabilities.

### Basic syntax

```pact
(env-sigs sigs)
```

### Arguments

Use the following argument when using the `env-sigs` Pact function.

| Argument | Type         | Description   |
|----------|--------------|---------------|
| `sigs`     | `[object]`   | Specifies the list of signature objects. Each object represents a signer `key` and its associated `caps` capabilities. |

### Return value

The `env-sigs` function returns a string indicating that the transaction signature keys and capabilities have been set.

### Example

The following example demonstrates setting transaction signature keys and capabilities within a Pact REPL:

```pact
(env-sigs [{'key: "my-key", 'caps: [(accounts.USER_GUARD "my-account")]}, {'key: "admin-key", 'caps: []}])
```
