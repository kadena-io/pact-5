## days

Use `days` to specify a number of days. 
You can use this function in conjunctions with the `add-time` function to add days to a specified time.

### Basic syntax

To specify a number of days, use the following syntax:

```pact
(days n)
```

### Arguments

Use the following argument to specify the `n` number of days for the `days` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `n` | decimal or integer | Specifies the number of days to add. |

### Return values

The `days` function returns the number of seconds in the given number of days as a decimal value.

### Example

The following example demonstrates how to use the `days` function to return the number of seconds in one day as a decimal value:

```pact
(days 1)
86400.0
```

The following example demonstrates how to use the `days` function in combination with `add-time`:

```pact
(add-time (time "2024-07-22T12:00:00Z") (days 1))
"2024-07-23T12:00:00Z"
```

In this example, `(days 1)` adds one day to the specified time "2024-07-22T12:00:00Z" using the `add-time` function, enabling straightforward manipulation of time-based information in smart contracts.
