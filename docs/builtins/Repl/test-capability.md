## test-capability

Use `test-capability` to acquire or install the `capability` specified.
You can use this function to acquire any capability that's not managed or to install any managed capability.
The specified capability and any composed capabilities it encompasses are in scope for the rest of the transaction.

### Basic syntax

To acquire or install the `capability` specified, use the following syntax:

```pact
(test-capability (capability))
```

## Arguments

Use the following argument when using the `env-module-admin` Pact function.

| Argument | Type | Description |
|----------|------|------------ |
| `capability` | capability-token | Specifies the capability and scope to test. |

### Return value

The `test-capability` function returns a string that indicates whether the capability has been installed or acquired.

### Example

The following example demonstrates how to use `test-capability` to acquire a capability for the scope of a REPL transaction:

```pact
pact> (module m g (defcap g () true))
"Loaded module m, hash c5kiBbxH0zDIMPpAlJdXwMNsNiwnZY4YRzkJvvagn1c"
pact> (test-capability (g))
"Capability acquired"
```
