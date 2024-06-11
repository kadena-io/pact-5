## env-keys (DEPRECATED)

**Note:** The `env-keys` function is deprecated in favor of `env-sigs`. It is recommended to use `env-sigs` for setting transaction signer keys with associated capabilities.

Use `env-keys` to set the transaction signer keys.

### Basic syntax

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

The following example demonstrates setting the transaction signer keys within a Pact REPL using `env-keys`:

```pactbash
pact> (env-keys ["my-key" "admin-key"])
"Setting transaction keys"
```

In this example, `env-keys` is called with a list of two keys: "my-key" and "admin-key". The function sets these keys as the current transaction signer keys.

The function returns a string indicating that the transaction keys have been set.
