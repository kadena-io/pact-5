## ceiling

Use `ceiling` to round up the value of a specified decimal `value` to the nearest integer or to a specified precision `prec` as a decimal.

### Basic syntax

To round up the value of a decimal `value` to the nearest integer, use the following syntax:

```pact
(ceiling value)
```

To round up the value of a decimal `value` to a specified `precision` as a decimal, use the following syntax:

```pact
(ceiling value precision)
```

### Arguments

Use the following arguments to specify the decimal `value` and optional `precision` for the `ceiling` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `value` | decimal | Specifies the decimal value to round up. |
| `precision` | integer | Specifies the precision to round the specified `value` to (optional). |

### Return values

The `ceiling` function returns the rounded-up value as an integer or as a decimal based on the input and precision.

### Examples

The following example rounds up a decimal value to the nearest integer in the Pact REPL:

```pact
pact> (ceiling 3.5)
4
```

The following example rounds up a decimal value to a precision of 2 decimal places:

```pact
pact> (ceiling 100.15234 2)
100.16
```

In this example, `ceiling` rounds up the decimal value `100.15234` to a precision of 2 decimal places, resulting in `100.16`.
