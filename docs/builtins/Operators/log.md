## log

Use `log` to compute the logarithm of the specified `value` with the specified `base`.

### Basic syntax

To compute the logarithm of a specified `value` with the specified `base`, use the following syntax:

```pact
(log base value)
```

### Arguments

Use the following arguments to specify the `base` and `value` for which you want to compute the logarithm using the `log` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `base` | integer or decimal | Specifies the base of the logarithm. |
| `value` | integer or decimal | Specifies the value for which you want to compute the logarithm. |

### Return value

The `log` function returns the logarithm of `value` with base `base`.

### Examples

The following example demonstrates how to use the `log` function to computer the logarithm of 256 with base 2:

```pact
pact> (log 2 256)
8
```
