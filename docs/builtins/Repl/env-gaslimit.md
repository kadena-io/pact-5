## env-gaslimit

Use `env-gaslimit` to set the environment gas limit to a specific value.

### Basic syntax

To set the environment gas limit to a specific value, use the following syntax:

```pact
(env-gaslimit limit)
```

### Arguments

Use the following argument to set the gas limit when using the `env-gaslimit` Pact function.

| Argument | Type | Description |
| -------- | ---- |------------ |
| `limit` | integer | Specifies the gas limit to set for the environment. |

### Return value

The `env-gaslimit` function returns a string indicating that the gas limit has been set to the specified value.

### Examples

The following example demonstrates how to use the `env-gaslimit` function to set a gas limit for the environment:

```pact
pact> (env-gaslimit 10)
"Set gas limit to 10"
```
