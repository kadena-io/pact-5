## format-time

Use `format-time` to format a `time` value using a specified `format`.
The `format-time` function is useful for converting time values to human-readable formats in Pact contracts.

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

## Time formats

The `parse-time` and `format-time` functions accept format codes that
derive from GNU `strftime` with some extensions, as follows:

`%%` - Literal percentage sign `"%"`.

`%z` - RFC 822/ISO 8601:1988 style numeric time zone, for example, `"-0600"` or `"+0100"`.

`%N` - ISO 8601 style numeric time zone, for example, `"-06:00"` or `"+01:00"`.

`%Z` - Timezone name.

`%c` - The preferred calendar time representation for the current locale. As
'dateTimeFmt' in `locale`, for example, `%a %b %e %H:%M:%S %Z %Y`.

`%R` - Same as `%H:%M`.

`%T` - Same as `%H:%M:%S`.

`%X` - The preferred time of day representation for the current locale. As
'timeFmt' in `locale`, for example, `%H:%M:%S`.

`%r` - The complete calendar time using the AM/PM format of the current locale. As 'time12Fmt' in `locale`, for example, `%I:%M:%S %p`.

`%P` - Day-half of day from 'amPm' in `locale`, converted to lowercase, for example, `"am"`, `"pm"`.

`%p` - Day-half of day from 'amPm' in `locale`, for example, `"AM"`, `"PM"`.

`%H` - Hour of day in 24-hour format, 0-padded to two characters, `"00"`–`"23"`.

`%k` - Hour of day in 24-hour format, space-padded to two characters, `" 0"`–`"23"`.

`%I` - Hour of day-half in 12-hour format, 0-padded to two characters, `"01"`–`"12"`.

`%l` - Hour of day-half in 12-hour format, space-padded to two characters, `" 1"`–`"12"`.

`%M` - Minute of the hour, 0-padded to two characterss, `"00"`–`"59"`.

`%S` - Second of minute without decimal part, 0-padded to two characters,
`"00"`–`"60"`.

`%v` - Microsecond of second, 0-padded to six characters, `"000000"`–`"999999"`.

`%Q` - Decimal point and fraction of second, up to 6 second decimals, without
trailing zeros. For a whole number of seconds, `%Q` produces the empty string.

`%s` - Number of whole seconds since the Unix epoch. For times before the UNIX epoch, this is a negative number. Note that in `%s.%q` and `%s%Q` formats, the decimals are positive, not negative. For example, 0.9 seconds before the UNIX epoch is formatted as `"-1.1"` with `%s%Q`.

`%D` - Same as `%m\/%d\/%y`.

`%F` - Same as `%Y-%m-%d`.

`%x` - As 'dateFmt' in `locale`, for example, `%m\/%d\/%y`.

`%Y` - Year, no padding.

`%y` - Year of century, 0-padded to two characters, `"00"`–`"99"`.

`%C` - Century, no padding.

`%B` - Month name, long form 'fst' from 'months' in `locale`,
`"January"`–`"December"`.

`%b`, `%h` - Month name, short form 'snd' from 'months' in `locale`,
`"Jan"`–`"Dec"`.

`%m` - Month of year, 0-padded to two characters, `"01"`–`"12"`.

`%d` - Day of month, 0-padded to two characters, `"01"`–`"31"`.

`%e` - Day of month, space-padded to two characters, `" 1"`–`"31"`.

`%j` - Day of year, 0-padded to three characters, `"001"`–`"366"`.

`%G` - Year for Week Date format, no padding.

`%g` - Year of century for Week Date format, 0-padded to two characters,
`"00"`–`"99"`.

`%f` - Century for Week Date format, no padding.

`%V` - Week of year for Week Date format, 0-padded to two characters, `"01"`–`"53"`.

`%u` - Day of week for Week Date format, `"1"`–`"7"`.

`%a` - Day of week, short form 'snd' from 'wDays' in `locale`, `"Sun"`–`"Sat"`.

`%A` - Day of week, long form 'fst' from 'wDays' in `locale`,
`"Sunday"`–`"Saturday"`.

`%U` - Week of year where weeks start on Sunday (as 'sundayStartWeek'), 0-padded to two characters, `"00"`–`"53"`.

`%w` - Day of week number, `"0"` (= Sunday) – `"6"` (= Saturday).

`%W` - Week of year where weeks start on Monday (as 'Data.Thyme.Calendar.WeekdayOfMonth.mondayStartWeek'), 0-padded to two characters, `"00"`–`"53"`.

Note: `%q` (picoseconds, zero-padded) does not work properly so not documented here.

### Default format and JSON serialization

The default format is a UTC ISO 8601 date time format: "%Y-%m-%dT%H:%M:%SZ", as accepted by the `time` function.
While the time object internally supports up to microsecond resolution, values returned from the Pact interpreter as JSON will be serialized with the default format. 
When higher resolution is desired, explicitly format times with `%v` and
related codes.

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