## parse-time

Use `parse-time` to construct a formatted time string from a UTC value using a specified format. 
The following table provides a summary of common time formatting codes:

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
For information about all of the formats supported, see [Time formats](/pact-5/Time/time-functions#time-formats).

### Basic syntax

To construct time from a UTC value using a specified format, use the following syntax:

`(parse-time format utcval)`

### Arguments

Use the following arguments to specify the format and UTC value for constructing time using the `parse-time` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `format` | string | Specifies the format for parsing the UTC value. |
| `utcval` | string | Specifies the UTC value to be parsed. |

### Return value

The `parse-time` function returns a time value constructed from the provided UTC value using the specified format.

### Examples

The following example demonstrates the use of `parse-time` in the Pact REPL:

```pact
pact> (parse-time "%F" "2024-11-06")
"2024-11-06T00:00:00Z"
```

In this example, `parse-time` is used to construct a time value using the the ISO 8601 standard specified by the "%F" format from the UTC value "2024-11-06".
