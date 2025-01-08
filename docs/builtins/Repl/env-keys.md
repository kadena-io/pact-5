## env-keys (DEPRECATED)

Use `env-keys` to set the transaction signer keys in older version of the Pact REPL.
The `env-keys` function is deprecated in favor of [`env-sigs`](/pact-5/repl/env-sigs). 
You should use `env-sigs` for setting transaction signer keys with associated capabilities.

### Basic syntax

To set transaction signer keys in older version of the Pact REPL, use the following syntax:

```pact
(env-keys keys)
```

## Arguments

Use the following argument when using the `env-keys` Pact function.

| Argument | Type     | Description                                                  |
|----------|----------|--------------------------------------------------------------|
| keys     | [string] | Specifies the list of keys to set as transaction signer keys. |

### Return value

The `env-keys` function returns a string indicating that the transaction keys have been set.

### Example

The following example demonstrates how to use the `env-keys` function to set "my-key" and "admin-key" as the current transaction signing keys in a Pact REPL:

```pact
pact> (env-keys ["my-key" "admin-key"])
"Setting transaction keys"
```
