## env-gas

Use `env-gas` to query the current gas state or set it to a specific value.

### Basic syntax

To query the current gas state, use the following syntax:

```pact
(env-gas)
```

To set the gas state to a specific value, use the following syntax:

```pact
(env-gas gas)
```

### Arguments

Use the following argument to set the gas state when using the `env-gas` Pact function.

| Argument | Type    | Description |
|----------|---------|-------------|
| `gas` | integer | Specifies the value to set the gas state to (optional). |

### Return value

When called without arguments, the `env-gas` function returns an integer representing the current gas state.

When called with the `gas` argument, the `env-gas` function returns a string indicating that the gas state has been set to the specified value.

### Examples

The following example demonstrates how to use the `env-gas` function to query the current gas state:

```pact
pact> (env-gas)
7
```

In the following example, the `env-gas` function resets the current gas state to zero:

```pact
pact> (env-gasmodel "table") (env-gaslimit 10) (env-gas 0) (map (+ 1) [1 2 3])
"Set gas model to table-based cost model"
"Set gas limit to 10"
"Set gas to 0"
[2 3 4]
```
