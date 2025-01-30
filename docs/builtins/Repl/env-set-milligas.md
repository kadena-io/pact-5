## env-set-milligas

Use `env-milligas` to set the current gas state to a specific value in milligas, similar to `env-gas` but 1 gas = 1e3 milligas

### Basic syntax

To set the current milligas state, use the following syntax:

```pact
(env-set-milligas milligas)
```

### Arguments

Use the following argument to set the gas state when using the `env-set-milligas` Pact function.

| Argument | Type    | Description |
|----------|---------|-------------|
| `milligas` | integer | Specifies the value to set the milligas state to |

### Return value

When called with the `milligasgas` argument, the `env-set-milligas` function returns a string indicating that the gas state has been set to the specified value.

### Examples

In the following example, the `env-set-milligas` function resets the current gas state to 0.01 gas:

```pact
pact> (env-set-milligas 10)
"Set milligas to 10"
```
