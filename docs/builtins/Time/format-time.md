## format-time

Use `format-time` to format a `time` value using a specified `format`.
The `format-time` function is useful for converting time values to human-readable formats in Pact contracts.

The `format-time` function accepts format codes that are derived from the formatting time `strftime` function time templates.
The following table provides a summary of the most common time formatting codes:

| Format | Purpose |
| --- | --- |
| %Y | Year with no padding. |
| %m | Month of the year, zero-padded to two characters, "01"–"12" |
| %d | Day of the month, zero-padded to two characters, "01"–"31" |
| %H | Hour of the day using a 24-hour clock, zero-padded to two characters, "00"–"23" |
| %M | Minute of of the hour, zero-padded to two characters, "00"–"59" |
| %S | Second of the minute, zero-padded to two characters, "00"–"60" |

There are many other formatting options.
For example, you can replace the numeric representing the month of the year with the short or long name for the month.
For information about all of the formats supported, see [Time formats](/pact-5/time/time-functions#time-formats).

Note that two percentage characters (`%%`) are interpreted as a literal percentage sign (`%`), not a time formatting template.

### Basic syntax

To format a time value using a specified format, use the following syntax:

```pact
(format-time format time)
```

### Arguments

Use the following arguments to specify the format and time for the `format-time` Pact function:

| Argument | Type | Description |
|--------- |------|------------ |
| `format` | string | Specifies the format string for the time. |
| `time` | time | Specifies the time value to format. |

### Return values

The `format-time` function returns a new string with the formatted time value.

### Examples

The following example demonstrates how to use the `format-time` function to format the time value `(time "2016-07-22T12:00:00Z")` using the specified format:

```pact
pact> (format-time "%F" (time "2016-07-22T12:00:00Z"))
"2016-07-22"
```

In this example, `"%F"` is the format string specifying the format of the output.
The result of this operation is a formatted string representing the date in the format `YYYY-MM-DD`. 

The following example demonstrates how to replace the numeric representing the month of the year with the short name for the month:

```pact
pact> (format-time "%Y-%b-%d" (time "2024-07-24T13:30:45Z"))
"2024-Jul-24"
```
