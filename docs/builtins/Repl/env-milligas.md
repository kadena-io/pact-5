## env-milligas

Use `env-milligas` to query the current milligasgas state or set it to a specific value, similar to `env-gas` but 1 gas = 1e3 milligas

### Basic syntax

To query the current milligas state, use the following syntax:

```pact
(env-milligas)
```

### Arguments

This function takes no arguments

### Return value

When called, the `env-milligas` function returns an integer representing the current gas state, in milligas.

### Examples

The following example demonstrates how to use the `env-milligas` function to query the current gas state:

```pact
pact> (env-milligas)
7123
```
