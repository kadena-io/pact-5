## dec

Use `dec` to convert a specified integer `value` to a decimal value.
This function can be useful if you need to work with decimal values in Pact but have integer inputs.

### Basic syntax

To convert a specified integer `value` to a decimal value, use the following syntax:

```pact
(dec value)
```

### Arguments

Use the following argument to specify the integer for the `dec` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `value` | integer | Specifies the integer to cast to a decimal value. |

### Return value

The `dec` function returns the specified integer as a decimal value.

### Example

The following example demonstrates how to use the `dec` function:

```pact
pact> (dec 3)
3.0
```
