## minutes

Use `minutes` to represent a duration of a specified number of minutes.
You can use this function in combination with the `add-time` function to add a specific number of minutes to a given time.

### Basic syntax

To represent a duration of N minutes, use the following syntax:

```pact
(minutes n)
```

### Argument

Use the following argument to specify the number of minutes for the duration using the `minutes` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `n` | `decimal` or `integer` | Specifies the number of minutes for the duration. |

### Return value

The `minutes` function returns the duration in decimal format.

### Examples

The following example demonstrates the use of `minutes` in combination with `add-time` in the Pact REPL:

```pact
pact> (add-time (time "2016-07-22T12:00:00Z") (minutes 1))
2016-07-22 12:01:00 UTC
```
