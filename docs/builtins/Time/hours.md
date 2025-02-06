## hours

Use the `hours` function to calculate a time duration in hours.
You can use this function in combination with the `add-time` function to add a specific number of hours to a given time.

### Basic syntax

The syntax for the `hours` function is as follows:

```pact
(hours n)
```

### Arguments

Use the following argument to specify the number of hours for the duration using the `hours` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `n` | integer or decimal | Specifies the number of hours as either a decimal or an integer. |

### Return value

The `hours` function returns a decimal value representing the specified number of hours.

### Examples

The following example illustrates how to use the `hours` function with the `add-time` function to add three hours to the time represented by the string `"2024-07-22T12:00:00Z"`.

```pact
(add-time (time "2024-07-22T12:00:00Z") (hours 3))
"2024-07-22T15:00:00Z"
```

You can also use the `hours` function to convert hours specified as a integer or decimal to return the equivalent time in seconds.
For example, you can specify hours as an integer to return the number of seconds:

```pact
pact> (hours 3)
10800.0
```

In the following example, the `hours` function converts the decimal value 2.5 hours to the equivalent time in seconds.

```pact
pact> (hours 2.5)
9000.0
```

The `hours` function is useful for performing time calculations in Pact contracts, such as adding or subtracting specific durations from timestamps.
