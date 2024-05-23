## list
Use `list` to create a list from the specified elements. Note that this function is deprecated in Pact 2.1.1 with support for literal lists.

### Basic syntax

To create a list from elements, use the following syntax:

list *elems*

### Argument

Use the following argument to specify the elements from which you want to create a list using the `list` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| elems | * | Specifies the elements to be included in the list. |

### Return value

The `list` function returns a list containing the specified elements.

### Examples

The following example demonstrates the use of `list` in the Pact REPL:

```lisp
pact>(list 1 2 3)
[1 2 3]
```

In this example, a list containing the elements 1, 2, and 3 is created using the `list` function.
