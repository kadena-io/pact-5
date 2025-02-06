## ln

Use `ln` to compute the natural logarithm of a specified `value`.

### Basic syntax

To compute the natural logarithm of a value, use the following syntax:

```pact
(ln value)
```

### Arguments

Use the following argument to specify the value for which you want to compute the natural logarithm using the `ln` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `value` | integer or decimal | Specifies the value for which you want to compute the natural logarithm. |

### Return value

The `ln` function returns the natural logarithm of the specified value.

### Examples

The following example demonstrates how to use the `ln` function to computer the natural logarithm for the value of 60 and round the result to 6 decimal places:

```pact
pact> (round (ln 60) 6)
4.094345
```
