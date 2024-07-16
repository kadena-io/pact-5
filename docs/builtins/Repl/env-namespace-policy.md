## env-namespace-policy

Use `env-namespace-policy` to install a managed namespace policy.

### Basic syntax

To install a managed namespace policy, use the following syntax:

```pact
(env-namespace-policy allow-root ns-policy-func)
```

### Arguments

Use the following arguments when using the `env-namespace-policy` Pact function.

| Argument | Type | Description |
|--------- |----- | ----------- |
| `allow-root` | bool | Specifies whether to allow root-level namespace creation. If set to `true`, root-level namespaces can be created. If set to `false`, root-level namespace creation is restricted.|
| `ns-policy-func` | function | Specifies the namespace policy function. This function takes two arguments: the namespace string (`ns`) and the namespace admin guard (`ns-admin`), and returns a boolean value indicating whether the namespace is allowed based on the policy. The function should return `true` if the namespace is allowed, and `false` if it is not. |

### Return value

The `env-namespace-policy` function returns a string indicating that the namespace policy has been installed.

### Example

The following example demonstrates how to use the `env-namespace-policy` function to install a namespace policy in a Pact REPL:

```pact
(env-namespace-policy true (my-ns-policy-func))
```
