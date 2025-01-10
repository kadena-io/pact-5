## parse-time

Use `parse-time` to construct a UTC formatted time string from an input time that uses the specified format.

The `parse-time` function accepts format codes that are derived from the formatting time `strftime` function time templates.
For information about all of the time formats and specifiers, see [Time formats](/pact-5/time/time-functions#time-formats).

The following table provides a summary of the most common time formatting codes:

| Format | Purpose |
| --- | --- |
| %Y | Year with no padding. |
| %m | Month of the year, zero-padded to two characters, "01"–"12" |
| %d | Day of the month, zero-padded to two characters, "01"–"31" |
| %H | Hour of the day using a 24-hour clock, zero-padded to two characters, "00"–"23" |
| %M | Minute of of the hour, zero-padded to two characters, "00"–"59" |
| %S | Second of the minute, zero-padded to two characters, "00"–"60" |

Note that two percentage characters (`%%`) are interpreted as a literal percentage sign (`%`), not a time formatting template.

### Basic syntax

To construct a UTC time string from a input time that uses a specified format, use the following syntax:

```pact
(parse-time format input)
```

### Arguments

Use the following arguments to specify the format and input time value for constructing time using the `parse-time` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `format` | string | Specifies the format for parsing the input time to construct the time as a UTC string. |
| `input` | string | Specifies the input time to be parsed. |

### Return value

The `parse-time` function returns a UTC formatted time constructed from the provided input value that uses the specified format.

### Examples

The following example demonstrates the use of `parse-time` in the Pact REPL:

```pact
pact> (parse-time "%F" "2024-11-06")
"2024-11-06T00:00:00Z"
```

In this example, the `parse-time` function constructs a UTC-formatted time value from the input value "2024-11-06".
The input string uses the `%Y-%m-%d` format specified in the ISO 8601 standard and identified with the "%F" format argument.

In the following example, the input string uses the %D and %T format specifiers for date and time:

```pact
(parse-time "%D %T" "11/07/24 08:09:10")
"2024-11-07T08:09:10Z"
```

The following example parses an input string that uses the abbreviated month name (%b), the day of the month as a zero-padded string (%d), and the full year (%Y) to a UTC time:

```pact
(parse-time "%b %d %Y %T" "Nov 07 2024 08:09:10")
"2024-11-07T08:09:10Z"
```