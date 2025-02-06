## distinct

Use `distinct` to return a list with duplicates removed from a homogeneous list of `values`. 
The original order of the values is preserved.

### Basic syntax

To return a list with duplicates removed from a list of `values`, use the following syntax:

```pact
(distinct [values])
```

### Arguments

Use the following argument to specify the list of `VALUES` for the `distinct` Pact function.

| Argument | Type        | Description                                    |
|----------|-------------|------------------------------------------------|
| `values` | [any] | Specifies the list of values that includes duplicates. |

### Return value

The `distinct` function returns a list with duplicates removed from the specified list of `values`.

### Examples

The following example demonstrates how to use the `distinct` function to remove duplicates from a list of numeric values:

```pact
pact>(distinct [3 3 1 1 2 2])
[3, 1, 2]
```

The function returns `[3 1 2]`, preserving the original order of the values.
