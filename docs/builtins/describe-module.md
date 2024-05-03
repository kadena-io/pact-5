## describe-module
Use `describe-module` to get metadata for a specified `MODULE`. This function returns an object with fields including `name`, `hash`, `blessed`, `code`, and `keyset`.

### Basic syntax

To get metadata for a `MODULE`, use the following syntax:

`(describe-module MODULE)`

### Arguments

Use the following argument to specify the `MODULE` for the `describe-module` Pact function.

| Argument | Type   | Description                                  |
|----------|--------|----------------------------------------------|
| `module`   | `string` | Specifies the name of the module to describe.|

### Return values

The `describe-module` function returns an object with metadata for the specified `MODULE`.

### Examples

The following example demonstrates the `describe-module` function:

```lisp
pact>(module m G (defcap G () true))
pact>(describe-module 'm)
{"hash":"0RpFOMAZ2787-fNFO6DokGf_V5WiSLMK10v4xnOymX0", "interfaces":[  ], "name":"m"}
```

In this example, `(describe-module 'm)` is used to get metadata for the module named 'm'. The function returns an object providing detailed information about the module.
