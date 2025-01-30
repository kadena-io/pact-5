## print

Use `print` to convert a `value` into a string and print it to the repl logger (usually standard out)

### Basic syntax

To convert a `value` into a string and print it (REPL only), use

```pact
(print value)
```

### Arguments

Use the following argument to specify the value to be stringified and printed using the `print` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `value` | any | The value to be stringified and printed |


### Return value

The `print` function returns the unit value `()`

### Examples

The following example demonstrates how to use the `print` function to stringify a value in a Pact REPL and print it:

```pact
pact> (do (print "hello world!") (+ 1 2))
"hello world!"
3
```
