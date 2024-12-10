## add-time

Use `add-time` to add a specified number of `seconds` to a given `time`.

### Basic syntax

To add `seconds` to a `time`, use the following syntax:

```pact
(add-time time seconds)
```

### Arguments

Use the following arguments to specify the `time` to which you want to add `seconds` using the `add-time` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `time` | time | Specifies the time to which you want to add `seconds`. |
| `seconds` | decimal or integer | Specifies the number of seconds to add to the `time`. |

### Return values

The `add-time` function returns the resulting time after adding the specified number of `seconds` to the specified `time`.

### Examples

The following example adds 120 seconds to the specified time "2024-06-22T12:00:00Z" in the Pact REPL:

```pact
pact> (add-time (time "2024-06-22T12:00:00Z") 120)
"2024-06-22T12:02:00Z"
```

In most cases, you use the `add-time` function in combination with other functions such `hours` or `minutes` as follows:

```pact
pact> (add-time (time "2024-06-22T12:00:00Z") (hours 1))
"2024-06-22T13:00:00Z"

(add-time (time "2024-06-26T12:00:00Z") (minutes 35))
"2024-06-26T12:35:00Z"
```
