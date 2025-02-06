## print

Use `print` to convert a `value` into a string and print it to the REPL logger (usually standard out).

### Basic syntax

To convert a `value` into a string and print it as REPL output, use the following syntax:

```pact
(print value)
```

### Arguments

Use the following argument to specify the value to be converted into a string and printed using the `print` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `value` | any | Specifies the value to be converted into a string and printed. |

### Return value

The `print` function returns the unit value `()`.

### Examples

The following example demonstrates how to use the `print` function to convert a value to a string in the Pact REPL and print it:

```pact
pact> (do (print "hello world!") (+ 1 2))
"hello world!"
3
```
