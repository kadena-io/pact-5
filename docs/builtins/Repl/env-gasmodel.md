## env-gasmodel

Use `env-gasmodel` to update or query the current gas model.

### Basic syntax

To update the gas model to a table-based cost model, use the following syntax:

```pact
(env-gasmodel model)
```

To update the gas model to a fixed-rate model with a specific rate, use the following syntax:

```pact
(env-gasmodel model rate)
```

To query the current gas model, use the following syntax:

```pact
(env-gasmodel)
```

### Arguments

Use the following arguments when using the `env-gasmodel` Pact function.

| Argument | Type    | Description                                                                  |
|----------|---------|------------------------------------------------------------------------------|
| `model`    | `string`  | Specifies the gas model to set. Supported values are "table" and "fixed".    |

### Return value

When called with the `model` argument, `env-gasmodel` returns a string indicating the updated gas model.

When called without arguments, `env-gasmodel` returns a string describing the current gas model.

### Examples

The following examples demonstrate updating and querying the gas model within a Pact REPL:

1. Querying the current gas model:
```bash
pact> (env-gasmodel)
"Current gas model is 'unitGasModel': GasModel with constant cost MilliGas 0"
```

2. Updating the gas model to a table-based cost model:
```bash
pact> (env-gasmodel 'table)
"Set gas model to table-based cost model"
```
