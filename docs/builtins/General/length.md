## length

Use `length` to return the number of elements in a list, a string, or an object.

### Basic syntax

To compute the length of a list, string, or object, use the following syntax:

```pact
(length arg)
```

### Argument

Use the following argument to specify the value for which you want to compute the length using the `length` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `arg` | list, string, or object | Specifies the list, string, or object that you want to compute the length of. |

### Return value

The `length` function returns an integer representing the length of the specified list, string, or object.

### Examples

The following example demonstrates calculating the length of the list `[1, 2, 3]` in the Pact REPL:

```pact
pact>(length [1 2 3])
3
```

The following example calculates the length of the string `"abcdefgh"`, resulting in 8.

```pact
pact>(length "abcdefgh")
8
```

The following example calculates the length of the object `{ "a": 1, "b": 2 }`, resulting in 2.

```pact
pact>(length { "a": 1, "b": 2 })
2
```
