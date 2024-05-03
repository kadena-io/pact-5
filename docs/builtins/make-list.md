Use `make-list` to create a list by repeating a specified value a certain number of times.

### Basic syntax

To create a list by repeating a value a specified number of times, use the following syntax:

make-list *length value*

### Arguments

Use the following arguments to specify the length of the list and the value to be repeated using the `make-list` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| length | integer | Specifies the length of the list to be created. |
| value | any | Specifies the value to be repeated to create the list. |

### Return value

The `make-list` function returns a list containing the specified value repeated the specified number of times.

### Examples

The following example demonstrates the use of `make-list` in the Pact REPL:

```lisp
pact>(make-list 5 true)
[true true true true true]
```

In this example, a list containing the value `true` repeated 5 times is created using the `make-list` function.
