## show

Use `show` to convert a specified `value` into a string.

### Basic syntax

To convert a `value` into a string, use the following syntax:

```pact
(show value)
```

### Arguments

Use the following argument to specify the value to be converted to a string using the `show` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `value` | any | Specifies the value to be converted to a string. |

### Return value

The `show` function returns the argument as a string.

### Examples

The following examples demonstrate how to use the `show` function to convert a value to a string in a Pact REPL:

```pact
pact> (show 1)
"1"

pact> (show [1 2 3])
"[1, 2, 3]"

pact> (show {"1":"hello", "2":"world"})
"{"1": "hello","2": "world"}"
```
