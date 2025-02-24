## time

Use the `time` function to construct a time object from a UTC value using the ISO 8601 standard Universal Time Coordinated (UTC) date time format: 

```text
%Y-%m-%dT%H:%M:%SZ
```

The UTC format is the default time format in Pact for all time-related functions.
Internally, the time object supports up to microsecond resolution.
However, the values returned from the Pact interpreter as JSON are serialized using the default format. 
If you need higher resolution, you can explicitly format times with the `%v` template and related codes.

### Basic syntax

To construct a time object from a UTC value, use the following syntax:

```pact
(time UTC)
```

### Arguments

Use the following argument to specify the UTC value for constructing the time object using the `time` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `UTC` | string | Specifies the UTC value in ISO 8601 format `(%Y-%m-%dT%H:%M:%SZ)`. |

### Return value

The `time` function returns a time object constructed from the provided UTC value.

### Examples

The following example demonstrates how to use the `time` function in the Pact REPL. 
This example constructs a time object from the UTC value `"2016-07-22T11:26:35Z"`:

```pact
pact> (time "2016-07-22T11:26:35Z")
2016-07-22 11:26:35 UTC
```

This example illustrates how to use the `time` function to create a time object from a UTC value using the ISO 8601 format in Pact.
