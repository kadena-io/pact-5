## list (deprecated)

Use `list` to create a list from the specified `elements`. 
This function has been deprecated (Pact 2.1.1).
Use the `list` literal data type.

### Basic syntax

To create a list from elements, use the following syntax:

```pact
(list elements)
```

### Argument

Use the following argument to specify the elements from which you want to create a list using the `list` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `elements` | any | Specifies the elements to be included in the list. |

### Return value

The `list` function returns a list containing the specified elements.

### Examples

The following example demonstrates how to use the `list` function in the Pact REPL:

```pact
pact> (list 1 2 3)
[1 2 3]
```
