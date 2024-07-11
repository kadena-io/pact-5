## round

Use the `round` function to round numbers to integers or decimals using banker's rounding.
The function returns an integer value if you don't specify `precision` as an argument.
If you specify the `precision` argument, the function returns a decimal value with the specified precision.

### Basic syntax

To round a decimal value to the nearest integer, use the following syntax:

```pact
(round number)
```

To round a decimal value to a specified precision, use the following syntax:

```pact
(round number precision)
```

### Arguments

Use the following arguments to specify the value to be rounded and, optionally, the precision to round to using the `round` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `number` | decimal | Specifies the decimal value to be rounded. |
| `precision` | integer | Specifies the precision to round to, if applicable (optional). |

### Return value

If no precision is specified, the `round` function returns the rounded value as an `integer`. 
If precision is specified, it returns the rounded value as a `decimal` value.

### Examples

The following examples demonstrate the usage of the `round` function within the Pact REPL.

To round the decimal value 3.5 to the nearest integer:

```pact
pact> (round 3.5)
4
```

To round the decimal value 100.15234 to 2 decimal places:

```pact
pact> (round 100.15234 2)
100.15
```
