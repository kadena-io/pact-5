## print

Use `print` to send a specified `value` as output to the REPL logger (usually standard out).

### Basic syntax

To print a specified `value` as REPL output, use the following syntax:

```pact
(print value)
```

### Arguments

Use the following argument to specify the value to be printed using the `print` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `value` | any | Specifies the value to be printed. |

### Return value

The `print` function returns the unit value `()`.

### Examples

The following example demonstrates how to use the `print` function to print a string in the terminal when using the Pact command-line interpreter interactively:

```pact
(typeof (print (+ "hello " "world")))
"hello world"
"unit"
```

Note that you can only use the `print` built-in function when using Pact command-line interpreter interactively or in tests written in `.repl` files.
You can't use the `print` function in Pact modules that you deploy on any network.