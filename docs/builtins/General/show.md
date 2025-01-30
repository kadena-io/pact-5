## show

Use `show` to convert a `value` into a string.

### Basic syntax

To convert a `value` into a string, use

```pact
(show value)
```

### Arguments

Use the following argument to specify the value to be stringified using the `show` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `value` | any | The value to be stringified |


### Return value

The `show` function returns the argument as a string

### Examples

The following example demonstrates how to use the `show` function to stringify a value in a Pact REPL:

```pact
pact> (show 1)
"1"
pact> (show [1 2 3])
"[1, 2, 3]"
pact> (show {"1":"hello", "2":"world"})
"{"1": "hello","2": "world"}"
```
