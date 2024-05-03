Use `format-time` to format a `TIME` value using a specified `FORMAT`. See the ["Time Formats" documentation](pact-reference.html#time-formats) for supported formats.

### Basic syntax

To format a time value using a specified format, use the following syntax:

format-time *format* *time* -> *result*

### Arguments

Use the following arguments to specify the format and time for the `format-time` Pact function:

| Argument  | Type   | Description                                      |
|-----------|--------|--------------------------------------------------|
| format    | string | Specifies the format string for the time.        |
| time      | time   | Specifies the time value to format.              |

### Return values

The `format-time` function returns a new string with the formatted time value.

### Examples

The following example demonstrates the `format-time` function:

```lisp
(format-time "%F" (time "2016-07-22T12:00:00Z"))
```

In this example, `"%F"` is the format string specifying the format of the output. The `format-time` function is used to format the time value `(time "2016-07-22T12:00:00Z")` using the specified format. The result of this operation is a formatted string representing the date in the format `YYYY-MM-DD`. The `format-time` function is useful for converting time values to human-readable formats in Pact contracts.
