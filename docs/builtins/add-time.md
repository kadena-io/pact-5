## add-time
Use `add-time` to add a specified number of `seconds` to a given `time`.

### Basic syntax

To add `seconds` to a `time`, use the following syntax:

`(add-time time seconds)`

### Arguments

Use the following arguments to specify the *`time`* to which you want to add *`seconds`* using the `add-time` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `time` | `time` | Specifies the time to which you want to add `seconds`. |
| `seconds` | `decimal` or `integer` | Specifies the number of seconds to add to the `time`. |

### Return values

The `add-time` function returns the resulting time after adding the specified `seconds` as a time.

### Examples

The following example adds 15 seconds to a specific time in the Pact REPL:

```lisp
pact>(add-time (time "2016-07-22T12:00:00Z") 15)
"2016-07-22T12:00:15Z"
```

In this example, `add-time` returns the time "2016-07-22T12:00:15Z" after adding 15 seconds to the specified time "2016-07-22T12:00:00Z".
