## env-gas

Use `env-gas` to query the current gas state or set it to a specific value.

### Basic syntax

To query the current gas state, use the following syntax:

`(env-gas)`

To set the gas state to a specific value, use the following syntax:

`(env-gas gas)`

### Arguments

Use the following argument to set the gas state when using the `env-gas` Pact function.

| Argument | Type    | Description                     |
|----------|---------|----------------------------------|
| `gas`      | `integer` | (Optional) Specifies the value to set the gas state to. |

### Return value

When called without arguments, `env-gas` returns an integer representing the current gas state.

When called with the `gas` argument, `env-gas` returns a string indicating that the gas state has been set to the specified value.

### Example

The following example demonstrates querying and setting the gas state within a Pact REPL:

```pact
pact> (env-gasmodel "table") (env-gaslimit 10) (env-gas 0) (map (+ 1) [1 2 3]) (env-gas)
7
```
