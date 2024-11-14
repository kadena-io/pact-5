## diff-time

Use `diff-time` to compute the difference between `time1` and `time2` in seconds.

### Basic syntax

To compute the difference between `time1` and `time2` in seconds, use the following syntax:

```pact
(diff-time time1 time2)
```

### Arguments

Use the following arguments to specify the times for the `diff-time` Pact function:

| Argument | Type | Description                                   |
|----------|------|-----------------------------------------------|
| `time1`    | time | Specifies the first time for the calculation.|
| `time2`    | time | Specifies the second time for the calculation.|

### Return values

The `diff-time` function returns the difference between `time1` and `time2` in seconds as a decimal.

### Examples

The following example demonstrates how to use the `diff-time` function to compute the difference between the times "16:00:00" and "09:30:00" in seconds:

```pact
pact> (diff-time (parse-time "%T" "16:00:00") (parse-time "%T" "09:30:00"))
23400.0
```

In this example, the function returns the result of this computation as a decimal, representing the time difference between the two specified times.
