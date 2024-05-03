## days
Use `days` to specify a number of days *`N`*, which can be used with 'add-time' to add or subtract days from a given time.

### Basic syntax

To specify a number of days *`N`*, use the following syntax:

days *`N`*

### Arguments

Use the following argument to specify the number of days *`N`* for the `days` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| n | decimal or integer | Specifies the number of days to add or subtract. |

### Return values

The `days` function returns the specified number of days as a decimal value.

### Example

The following example demonstrates the `days` function in combination with 'add-time':

```lisp
(add-time (time "2016-07-22T12:00:00Z") (days 1))
```

In this example, `(days 1)` is used to specify 1 day, which is then added to the time "2016-07-22T12:00:00Z" using 'add-time'. This results in the time one day after the specified time. The `days` function allows for easy manipulation of time by specifying a number of days to add or subtract.
