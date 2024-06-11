## env-namespace-policy

Use `env-namespace-policy` to install a managed namespace policy.

### Basic syntax

```pact
(env-namespace-policy allow-root ns-policy-fun)
```

### Arguments

Use the following arguments when using the `env-namespace-policy` Pact function.

| Argument      | Type                                | Description   |
|---------------|-------------------------------------|---------------|
| `allow-root`    | `bool`                            | Specifies whether to allow root-level namespace creation. If set to `true`, root-level namespaces can be created. If set to `false`, root-level namespace creation is restricted.                                                                                                                                                                                                                                            |
| `ns-policy-fun` | `ns:string ns-admin:guard -> bool` | Specifies the namespace policy function. This function takes two arguments: `ns` (the namespace string) and `ns-admin` (the namespace admin guard), and returns a boolean value indicating whether the namespace is allowed based on the policy. The function should return `true` if the namespace is allowed, and `false` otherwise. |

### Return value

The `env-namespace-policy` function returns a string indicating that the namespace policy has been installed.

### Example

The following example demonstrates installing a namespace policy within a Pact REPL:

```pact
(env-namespace-policy (my-ns-policy-fun))
```
