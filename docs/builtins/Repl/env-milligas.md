## env-milligas

Use `env-milligas` to query the current gas state in units of one one thousandth of a gas unit.
This function is similar to the `env-gas` function, except that one (1) unit of gas is equal to one thousand units of milligas.
This function is useful when writing smart contracts with minimal resource consumption to measure gas costs in smaller units than gas.

### Basic syntax

To query the current gas state in units equal to one thousandth of a single unit of gas, use the following syntax:

```pact
(env-milligas)
```

### Arguments

The `env-milligas` function takes no arguments.

### Return value

When called, the `env-milligas` function returns an integer representing the current gas state, in units equal to one thousandth of a single unit of gas (milligas).

### Examples

The following example demonstrates how to use the `env-milligas` function to query the current gas state:

```pact
pact> (env-milligas)
7123
```
