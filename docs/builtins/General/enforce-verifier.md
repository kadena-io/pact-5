## enforce-verifier
Use `enforce-verifier` to enforce that a verifier with the specified `VERIFIERNAME` is in scope.

### Basic syntax

To enforce that a verifier is in scope, use the following syntax:

`(enforce-verifier VERIFIERNAME)`

### Arguments

Use the following argument to specify the `VERIFIERNAME` for the `enforce-verifier` Pact function:

| Argument    | Type   | Description                                     |
|-------------|--------|-------------------------------------------------|
| `VERIFIERNAME` | `string` | Specifies the name of the verifier to enforce.  |

### Return values

The `enforce-verifier` function returns a boolean value indicating whether the specified verifier is in scope.

### Examples

The following example demonstrates the `enforce-verifier` function:

```pact
pact>(enforce-verifier 'COOLZK)
<interactive>:0:0:Error: Verifier failure COOLZK: not in transaction
```

In this example, `(enforce-verifier 'COOLZK)` is used to enforce that the verifier named 'COOLZK' is in scope. If the verifier 'COOLZK' is in scope, the function returns true. If the verifier is not in scope, the function fails. The `enforce-verifier` function provides a way to ensure that a specific verifier is available for use within a Pact contract.
