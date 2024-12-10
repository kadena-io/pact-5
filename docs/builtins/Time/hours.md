## hours

Use the `hours` function to calculate a time duration in hours.
You can use this function in combination with the `add-time` function to add a specific number of hours to a given time.

### Basic syntax

The syntax for the `hours` function is as follows:

`(hours n)`

### Arguments

| Argument | Type | Description |
| --- | --- | --- |
| `n` | `integer` or `decimal` | Specifies the number of hours as either a decimal or an integer. |

### Return Value

The `hours` function returns a decimal value representing the specified number of hours.

### Examples

Adding hours to a time:

```pact
(add-time (time "2016-07-22T12:00:00Z") (hours 1))
```

In this example, the `add-time` function is used to add one hour to the time represented by the string `"2016-07-22T12:00:00Z"`.

Specifying hours as an integer:

```pact
pact>(hours 3)
10800.0
```

In this example, the `hours` function specifies 3 hours as an integer value.

Specifying hours as a decimal:

```pact
pact>(hours 2.5)
9000.0
```

In this example, the `hours` function specifies 2.5 hours as a decimal value.

The `hours` function is useful for performing time calculations in Pact contracts, such as adding or subtracting specific durations from timestamps.
