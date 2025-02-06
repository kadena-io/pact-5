## exp

Use `exp` to calculate the exponential function of the specified `value`.

### Basic syntax

To calculate the exponential function of a value, use the following syntax:

`(exp value)`

### Arguments

Use the following argument to specify the value for the `exp` Pact function:

| Argument | Type             | Description                                 |
|----------|------------------|---------------------------------------------|
| `value` | integer or decimal | Specifies the value for which to calculate the exponential function. |

### Return value

The `exp` function returns the exponential function of the specified value.

### Examples

The following example demonstrates how to use the `exp` function to calculate the exponential value for three and round the result of this calculation to precision of six decimal places:

```pact
pact> (round (exp 3) 6)
20.085537
```
