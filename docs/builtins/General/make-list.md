## make-list

Use `make-list` to create a list by repeating a specified value a certain number of times.

### Basic syntax

To create a list by repeating a `value` a specified number of `times`, use the following syntax:

```pact
(make-list times value)
```

### Arguments

Use the following arguments to specify the length of the list and the value to be repeated using the `make-list` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `times` | integer | Specifies the length of the list to be created. |
| `value` | any | Specifies the value to be repeated to create the list. |

### Return value

The `make-list` function returns a list containing the specified value repeated the specified number of times.

### Examples

The following example demonstrates how to use the `make-list` function to create a list containing the value `true` five times in the Pact REPL:

```pact
pact> (make-list 5 true)
[true true true true true]
```
