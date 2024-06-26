## format-time
Use `format-time` to format a `time` value using a specified `format`.

### Basic syntax

To format a time value using a specified format, use the following syntax:

format-time format time

### Arguments

Use the following arguments to specify the format and time for the `format-time` Pact function:

| Argument  | Type   | Description                                      |
|-----------|--------|--------------------------------------------------|
| `format`    | `string` | Specifies the format string for the time.        |
| `time`      | `time`   | Specifies the time value to format.              |

## Time formats

The `parse-time` and `format-time` functions accept format codes that
derive from GNU `strftime` with some extensions, as follows:

`%%` - literal `"%"`

`%z` - RFC 822/ISO 8601:1988 style numeric time zone (e.g., `"-0600"` or
`"+0100"`)

`%N` - ISO 8601 style numeric time zone (e.g., `"-06:00"` or `"+01:00"`)
/EXTENSION/

`%Z` - timezone name

`%c` - The preferred calendar time representation for the current locale. As
'dateTimeFmt' `locale` (e.g. `%a %b %e %H:%M:%S %Z %Y`)

`%R` - same as `%H:%M`

`%T` - same as `%H:%M:%S`

`%X` - The preferred time of day representation for the current locale. As
'timeFmt' `locale` (e.g. `%H:%M:%S`)

`%r` - The complete calendar time using the AM/PM format of the current locale.
As 'time12Fmt' `locale` (e.g. `%I:%M:%S %p`)

`%P` - day-half of day from ('amPm' `locale`), converted to lowercase, `"am"`,
`"pm"`

`%p` - day-half of day from ('amPm' `locale`), `"AM"`, `"PM"`

`%H` - hour of day (24-hour), 0-padded to two chars, `"00"`–`"23"`

`%k` - hour of day (24-hour), space-padded to two chars, `" 0"`–`"23"`

`%I` - hour of day-half (12-hour), 0-padded to two chars, `"01"`–`"12"`

`%l` - hour of day-half (12-hour), space-padded to two chars, `" 1"`–`"12"`

`%M` - minute of hour, 0-padded to two chars, `"00"`–`"59"`

`%S` - second of minute (without decimal part), 0-padded to two chars,
`"00"`–`"60"`

`%v` - microsecond of second, 0-padded to six chars, `"000000"`–`"999999"`.
/EXTENSION/

`%Q` - decimal point and fraction of second, up to 6 second decimals, without
trailing zeros. For a whole number of seconds, `%Q` produces the empty string.
/EXTENSION/

`%s` - number of whole seconds since the Unix epoch. For times before the Unix
epoch, this is a negative number. Note that in `%s.%q` and `%s%Q` the decimals
are positive, not negative. For example, 0.9 seconds before the Unix epoch is
formatted as `"-1.1"` with `%s%Q`.

`%D` - same as `%m\/%d\/%y`

`%F` - same as `%Y-%m-%d`

`%x` - as 'dateFmt' `locale` (e.g. `%m\/%d\/%y`)

`%Y` - year, no padding.

`%y` - year of century, 0-padded to two chars, `"00"`–`"99"`

`%C` - century, no padding.

`%B` - month name, long form ('fst' from 'months' `locale`),
`"January"`–`"December"`

`%b`, `%h` - month name, short form ('snd' from 'months' `locale`),
`"Jan"`–`"Dec"`

`%m` - month of year, 0-padded to two chars, `"01"`–`"12"`

`%d` - day of month, 0-padded to two chars, `"01"`–`"31"`

`%e` - day of month, space-padded to two chars, `" 1"`–`"31"`

`%j` - day of year, 0-padded to three chars, `"001"`–`"366"`

`%G` - year for Week Date format, no padding.

`%g` - year of century for Week Date format, 0-padded to two chars,
`"00"`–`"99"`

`%f` - century for Week Date format, no padding. /EXTENSION/

`%V` - week of year for Week Date format, 0-padded to two chars, `"01"`–`"53"`

`%u` - day of week for Week Date format, `"1"`–`"7"`

`%a` - day of week, short form ('snd' from 'wDays' `locale`), `"Sun"`–`"Sat"`

`%A` - day of week, long form ('fst' from 'wDays' `locale`),
`"Sunday"`–`"Saturday"`

`%U` - week of year where weeks start on Sunday (as 'sundayStartWeek'), 0-padded
to two chars, `"00"`–`"53"`

`%w` - day of week number, `"0"` (= Sunday) – `"6"` (= Saturday)

`%W` - week of year where weeks start on Monday (as
'Data.Thyme.Calendar.WeekdayOfMonth.mondayStartWeek'), 0-padded to two chars,
`"00"`–`"53"`

Note: `%q` (picoseconds, zero-padded) does not work properly so not documented
here.

### Default format and JSON serialization

The default format is a UTC ISO8601 date+time format: "%Y-%m-%dT%H:%M:%SZ", as
accepted by the `time` function.
While the time object internally supports up to microsecond resolution, values
returned from the Pact interpreter as JSON will be serialized with the default
format. When higher resolution is desired, explicitly format times with `%v` and
related codes.

### Return values

The `format-time` function returns a new string with the formatted time value.

### Examples

The following example demonstrates the `format-time` function:

```pact
pact>(format-time "%F" (time "2016-07-22T12:00:00Z"))
"2016-07-22"
```

In this example, `"%F"` is the format string specifying the format of the output. The `format-time` function is used to format the time value `(time "2016-07-22T12:00:00Z")` using the specified format. The result of this operation is a formatted string representing the date in the format `YYYY-MM-DD`. The `format-time` function is useful for converting time values to human-readable formats in Pact contracts.
