## env-set-debug-flag

Use `env-set-debug-flag` to set the pact interpreter debug flags, which will spit out the result of the internal tree transformations that the pact interpreter does. Note: this function lets you inspect pact trees from modules and terms in the way the CEK machine understands them, and is thus primarily suitable for advanced debugging.

### Basic syntax

To set the pact repl debug flags, use

```pact
(env-set-debug-flag "flag")
```

### Arguments

Use the following argument when using the `env-set-debug-flag` Pact function.

| Argument | Type         | Description   |
|----------|--------------|---------------|
| `flag`  | string | Specifies the debug flag to set|

Currently valid debug flags:

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
