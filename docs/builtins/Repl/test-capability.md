## test-capability

Acquire (if unmanaged) or install (if managed) CAPABILITY. CAPABILITY and any
  composed capabilities are in scope for the rest of the transaction.

### Basic syntax

```pact
(test-capability (MY_CAPABILITY))
```

## Arguments

Use the following argument when using the `env-module-admin` Pact function.

| Argument | Type     | Description                                                  |
|----------|----------|--------------------------------------------------------------|
| capability | cap-token | Specifies the capability and scope to test |


### Return value

Returns a string that indicates whether the capability has been installed (managed) or acquired (unmanaged).

### Example

The following example demonstrates how to use `test-capability` to acquire an unmanaged capability for the scope of a repl transaction

```pact
pact> (module m g (defcap g () true))
"Loaded module m, hash c5kiBbxH0zDIMPpAlJdXwMNsNiwnZY4YRzkJvvagn1c"
pact> (test-capability (g))
"Capability acquired"
```
