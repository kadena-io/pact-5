## days
Use `days` to specify a number of days `N`, which can be used with 'add-time' to add days from a given time.

### Basic syntax

To specify a number of days `N`, use the following syntax:

```pact
(days N)
```

### Arguments

Use the following argument to specify the number of days `N` for the `days` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `n` | `decimal` or `integer` | Specifies the number of days to add. |

### Return values

The `days` function returns the number of seconds in the given number of days, as a decimal value.

### Example

The following example demonstrates the `days` function in combination with `add-time`:

```pact
(add-time (time "2016-07-22T12:00:00Z") (days 1))
"2016-07-23T12:00:00Z"
```

In this example, `(days 1)` is used to specify 1 day, which is then added to the time "2016-07-22T12:00:00Z" using `add-time`. This results in the time one day after the specified time. The `days` function allows for easy manipulation of time by specifying a number of days to add.
