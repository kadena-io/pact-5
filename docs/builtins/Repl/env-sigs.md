## env-sigs

Use `env-sigs` to set signature keys for signing transactions and granting capabilities.

### Basic syntax

To set the signature keys to use for signing transactionL and granting capabilities, use the following syntax:

```pact
(env-sigs sigs)
```

### Arguments

Use the following argument when using the `env-sigs` Pact function.

| Argument | Type         | Description   |
|----------|--------------|---------------|
| `sigs`  | [object] | Specifies the list of signature objects. Each object represents a signer `key` and its associated `caps` capabilities. |

### Return value

The `env-sigs` function returns a string indicating that the transaction signature keys and capabilities have been set.

### Examples

The following example demonstrates how to use the `env-sigs` function to set two transaction signature keys—"my-key" and "admin-key"—and capabilities:

```pact
(env-sigs [
    {'key: "my-key", 'caps: [(accounts.USER_GUARD "my-account")]
    }, 
    {'key: "admin-key", 'caps: []}
    ])
```

The following example illustrates using the `env-sigs` function to grant "any" key the MINT capability:

```pact
(env-sigs [
    { 'key: 'any
     ,'caps: [(MINT (read-msg "token-id") (read-string 'account) 1.0)]}
   ])
   ```