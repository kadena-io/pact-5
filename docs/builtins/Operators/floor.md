## floor

Use `floor` to round down the value of a decimal `value` to an integer, or to a specified `precision` number of decimal places.
The `floor` function is useful for situations where you need to round down decimal values in Pact contracts.

### Basic syntax

To round down a decimal value to an integer, use the following syntax:

```pact
(floor value)
```

To round down a decimal value to a specified precision, use the following syntax:

```pact
(floor value precision)
```

### Arguments

Use the following arguments to specify the decimal value and precision for the `floor` Pact function:

| Argument | Type | Description |
|----------|------|-------------|
| `value` | decimal | Specifies the decimal value to round down. |
| `precision` | integer | Specifies the precision to round down to for the resulting decimal value (optional). |

### Return values

The `floor` function returns the rounded-down value of the specified decimal:

- If only `value` is provided, it returns an integer.
- If `precision` is provided, it returns a decimal with the specified precision.

### Examples

The following example demonstrates how to use the `floor` function to round down the `3.5` decimal value to the nearest integer:

```pact
pact> (floor 3.5)
3
```

The following example demonstrates how to use the `floor` function to round down the decimal value `100.15234` to a decimal value with a precision of two decimal places:

```pact
pact> (floor 100.15234 2)
100.15
```

The following example uses the floor function in an expression for calculating a royalty payout:

```pact
(royalty-payout:decimal
  (floor (* sale-price royalty-rate) (fungible::precision))
)
```