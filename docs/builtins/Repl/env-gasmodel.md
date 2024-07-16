## env-gasmodel

Use `env-gasmodel` to query or update the current gas model.

### Basic syntax

To query the current gas model, use the following syntax:

```pact
(env-gasmodel)
```

To update the gas model to a table-based cost model, use the following syntax:

```pact
(env-gasmodel model)
```

To update the gas model to a fixed-rate model with a specific rate, use the following syntax:

```pact
(env-gasmodel model rate)
```


### Arguments

Use the following arguments when using the `env-gasmodel` Pact function.

| Argument | Type | Description |
| -------- |----- |------------ |
| `model` | string | Specifies the gas model to set. The supported gas models are `table` to use a table-based gas model and `fixed` to use a fixed rate gas model.|
| `rate` | integer | Specifies the fixed rate or every operation. This argument is required if you set the `model` argument to use the `fixed` gas model.

### Return value

When called with the `model` argument, `env-gasmodel` returns a string indicating the updated gas model.

When called without arguments, `env-gasmodel` returns a string describing the current gas model.

### Examples

The following example demonstrates how to use the `env-gasmodel` function to query the current gas model:

```pact
pact> (env-gasmodel)
"Current gas model is 'fixed 1': constant rate gas model with fixed rate 1"
```

The following example demonstrates how to use the `env-gasmodel` function to update the current gas model to use a table-based cost model:

```pact
pact> (env-gasmodel "table")
"Set gas model to table-based cost model"
```

In the following example, the env-gasmodel function updates the gas model to use a fixed rate of two:

```pact
pact> (env-gasmodel "fixed" 2)
"Set gas model to constant rate gas model with fixed rate 2"
```
