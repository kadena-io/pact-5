## map

Use `map` to apply an application function `app` to each element in a `list`, returning a new list of results.

### Basic syntax

To apply an application function to each element in a list, use the following syntax:

```pact
(map app [list])
```

### Arguments

Use the following arguments to specify the application function and the list of elements to be mapped using the `map` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `app` | function `x:<b> -> <a>` | Specifies the application function to be applied to each element in the list. |
| `list` | [any] | Specifies the list of elements to be mapped. |

### Return value

The `map` function returns a new list containing the results of applying the application function to each element in the input list.

### Examples

The following example demonstrates how to use the `map` function to apply `(+ 1)` to each element in the specified list in the Pact REPL:

```pact
pact> (map (+ 1) [1 2 3])
[2 3 4]
```

When the application function `(+ 1)` is applied to each element in the list, the function returns a new list with the values `[2 3 4]`.
