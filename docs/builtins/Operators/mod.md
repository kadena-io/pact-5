## mod

Use `mod` to compute the remainder of `X` divided by `Y`.

### Basic syntax

To compute the remainder of `X` divided by `Y`, use the following syntax:

`(mod X Y)`

### Arguments

Use the following arguments to specify the integers for which you want to compute the remainder using the `mod` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `X` | `integer` | Specifies the dividend. |
| `Y` | `integer` | Specifies the divisor. |

### Return value

The `mod` function returns the remainder of the division of `X` by `Y`.

### Examples

The following example demonstrates the use of `mod` in the Pact REPL:

```pact
pact>(mod 13 8)
5
```

In this example, the remainder of 13 divided by 8 is computed, resulting in 5.
