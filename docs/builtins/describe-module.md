Use `describe-module` to get metadata for a specified *`MODULE`*. This function returns an object with fields including 'name', 'hash', 'blessed', 'code', and 'keyset'.

### Basic syntax

To get metadata for a *`MODULE`*, use the following syntax:

describe-module *`MODULE`*

### Arguments

Use the following argument to specify the *`MODULE`* for the `describe-module` Pact function.

| Argument | Type   | Description                                  |
|----------|--------|----------------------------------------------|
| module   | string | Specifies the name of the module to describe.|

### Return values

The `describe-module` function returns an object with metadata for the specified *`MODULE`*.

### Examples

The following example demonstrates the `describe-module` function:

```lisp
(describe-module 'my-module)
```

In this example, `(describe-module 'my-module)` is used to get metadata for the module named 'my-module'. The function returns an object with fields such as 'name', 'hash', 'blessed', 'code', and 'keyset', providing detailed information about the module.
