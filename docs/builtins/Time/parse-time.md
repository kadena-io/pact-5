## parse-time

Use `parse-time` to construct a UTC formatted time string from an input time that uses the specified format.

The `parse-time` and `format-time` functions accept format codes that are derived from the formatting time `strftime` function time templates with some extensions.
You can use the following template specifiers that are introduced by using a single percentage (`%`) character to format time strings:

| Specifier | Description |
| :-------- | :---------- |
| `%a` | The abbreviated short name for the day of the week as specified for the current locale, for example,`"Sun"`–`"Sat"`. |
| `%A` | The full name for the day of the week as specified for the current locale, for example, `"Sunday"`–`"Saturday"`. |
| `%b` | The abbreviated month name as specified for the current locale, for example, `"Jan"`–`"Dec"`. |
| `%B` | The full month name as specified for the current locale, for example, `"January"`–`"December"`. |
| `%c` | The preferred calendar time representation as specified for the current locale, for example, `%a %b %e %H:%M:%S %Z %Y`.|
| `%C` | The century of the year with no padding.|
| `%d` | The day of the month as a decimal number zero-padded to two characters, for example, `"01"`–`"31"`.|
| `%D` | The date using the format `%m/%d/%y`.|
| `%e` | The day of the month as a decimal number space-padded to two characters, for example, `" 1"`–`"31"`.|
| `%f` | Century for the week date format with no padding.|
| `%F` | The date using the format `%Y-%m-%d` as specified in the ISO 8601 standard.|
| `%g` | The year corresponding to the ISO week number, without the century, zero-padded to two characters, for example, `"00"`–`"99"`.|
| `%G` | The year corresponding to the ISO week number with no padding.|
| `%h` | The abbreviated month name as specified for the current locale, for example, `"Jan"`–`"Dec"`. This format is the same as using the `%b` template.|
| `%H` | The hour of the day as a decimal number, using a 24-hour clock, zero-padded to two characters, for example, `"00"`–`"23"`.|
| `%I` | The hour of the day as a decimal number, using a 12-hour clock, zero-padded to two characters, for example, `"01"`–`"12"`.|
| `%j` | The day of the year as a decimal number, zero-padded to three characters, for example, `"001"`–`"366"`.|
| `%k` | The hour of the day as a decimal number, using a 24-hour clock, space-padded to two characters, for example, `" 0"`–`"23"`.|
| `%l` | The hour as a decimal number using a 12-hour clock, space-padded to two characters, for example, `" 1"`–`"12"`.|
| `%m` | The month of the year, zero-padded to two characters, for example, `"01"`–`"12"`.|
| `%M` | The minute of the hour, zero-padded to two characters, for example, `"00"`–`"59"`.|
| `%N` | Numeric representation of the time zone using the ISO 8601 standard, for example, `"-06:00"` or `"+01:00"`.|
| `%p` | The half of day representation of `"AM"` or `"PM"` or the corresponding strings specified for the current locale. Noon is treated as ‘PM’ and midnight as ‘AM’. If `"AM"` and `"PM"` aren't supported, the `%p` template returns an empty string.|
| `%P` | The half of day representation of `"AM"` or `"PM"` or the corresponding strings specified for the current locale, converted to lowercase, for example, `"am"` and `"pm"`. Noon is treated as ‘PM’ and midnight as ‘AM’. If `"am"` and `"pm"` aren't supported, the `%P` template returns an empty string.|
| `%Q` | The fraction of a second, up to six second decimal places, without trailing zeros. For a whole number of seconds, `%Q` produces an empty string.|
| `%r` | The complete calendar time including the AM/PM format as specified for the current locale, for example, `%I:%M:%S %p`.|
| `%R` | The hour and minute in decimal numbers using the format `%H:%M`.|
| `%s` | The number of whole seconds since the UNIX epoch (since 1970-01-01 00:00:00 UTC). For times before the UNIX epoch, this is a negative number. Note that in `%s.%q` and `%s%Q` formats, the decimals are positive, not negative. For example, 0.9 seconds before the UNIX epoch is formatted as `"-1.1"` with `%s%Q`.|
| `%S` | The seconds of a minute, zero-padded to two characters, for example, `"00"`–`"60"`.|
| `%T` | The time of day using the format `%H:%M:%S`.|
| `%u` | The day of the week as a number with Monday being 1, for example, `"1"`–`"7"`.|
| `%U` | The week number of the year as a number, starting with the first Sunday as the first day of the first week, zero-padded to two characters, for example, `"00"`–`"53"`.|
| `%v` | The microsecond of a second, zero-padded to six characters, for example, `"000000"`–`"999999"`.|
| `%V` | The week number as specified using the ISO 8601:1988 standard where weeks start with Monday and end with Sunday. zero-padded to two characters, for example,`"01"`–`"53"`.|
| `%w` | The day of the week as a number, starting with Sunday as 0, for example,`"0"` (= Sunday) – `"6"` (= Saturday).|
| `%W` | The week number of the year as a number (range 00 through 53), starting with the first Monday as the first day of the first week. zero-padded to two characters, for example, `"00"`–`"53"`.|
| `%x` | The preferred date representation as specified for the current `locale`, for example, `%m\/%d\/%y`.|
| `%X` | The preferred time of day representation as specified for the current locale, for example, `%H:%M:%S`.|
| `%y` | The year without a century as a number, zero-padded to two characters, for example, `"00"`–`"99"`.|
| `%Y` | The full year as a number with no padding.|
| `%z` | Numeric representation of the time zone using the RFC 822/ISO 8601:1988 standard, for example, `"-0600"` or `"+0100"`.|
| `%Z` | The time zone abbreviation or empty if the time zone can't be determined.|

Note that two percentage characters (`%%`) are interpreted as a literal percentage sign (`%`), not a time formatting template.
In addition, the template for picoseconds with zero padding (`%q`) doesn't work properly, so it's not included as a supported template for formatting time strings.

<!-- NEXT GEN DOCS
The following table provides a summary of common time formatting codes:

| Format | Purpose |
| --- | --- |
| %Y | Year with no padding. |
| %m | Month of the year, zero-padded to two characters, "01"–"12" |
| %d | Day of the month, zero-padded to two characters, "01"–"31" |
| %H | Hour of the day using a 24-hour clock, zero-padded to two characters, "00"–"23" |
| %M | Minute of of the hour, zero-padded to two characters, "00"–"59" |
| %S | Second of the minute, zero-padded to two characters, "00"–"60" |

For more information about time formats and specifiers, see [Time formats](/pact-5/Time/time-functions#time-formats).
-->

### Basic syntax

To construct a UTC time string from a input time that uses a specified format, use the following syntax:

`(parse-time format input)`

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