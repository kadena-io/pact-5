## distinct
Use `distinct` to return a list with duplicates removed from a homogeneous list of *`VALUES`*. The original order of the values is preserved.

### Basic syntax

To return a list with duplicates removed from a list of *`VALUES`*, use the following syntax:

distinct *`VALUES`*

### Arguments

Use the following argument to specify the list of *`VALUES`* for the `distinct` Pact function.

| Argument | Type        | Description                                    |
|----------|-------------|------------------------------------------------|
| values   | [<a>]      | Specifies the list of values with duplicates. |

### Return values

The `distinct` function returns a list with duplicates removed from the specified list of *`VALUES`*.

### Examples

The following example demonstrates the `distinct` function:

```lisp
(distinct [3 3 1 1 2 2])
```

In this example, `(distinct [3 3 1 1 2 2])` is used to remove duplicates from the list `[3 3 1 1 2 2]`. The function returns `[3 1 2]`, which is the original list with duplicates removed while preserving the original order of the values.
