## diff-time
Use `diff-time` to compute the difference between `TIME1` and `TIME2` in seconds.

### Basic syntax

To compute the difference between two times `TIME1` and `TIME2` in seconds, use the following syntax:

`(diff-time TIME1 TIME2)`

### Arguments

Use the following arguments to specify the times for the `diff-time` Pact function:

| Argument | Type | Description                                   |
|----------|------|-----------------------------------------------|
| `TIME1`  | `time` | Specifies the first time for the calculation.|
| `TIME2`    | `time` | Specifies the second time for the calculation.|

### Return values

The `diff-time` function returns the difference between `TIME1` and `TIME2` in seconds as a decimal.

### Examples

The following example demonstrates the `diff-time` function:

```lisp
(diff-time (parse-time "%T" "16:00:00") (parse-time "%T" "09:30:00"))
```

In this example, `(diff-time (parse-time "%T" "16:00:00") (parse-time "%T" "09:30:00"))` is used to compute the difference between the times "16:00:00" and "09:30:00" in seconds. The function returns the result of this computation as a decimal, representing the time difference between the two specified times.
