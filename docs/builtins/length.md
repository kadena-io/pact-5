## length
Use `length` to compute the length of X, where X can be a list, a string, or an object.

### Basic syntax

To compute the length of a list, string, or object, use the following syntax:

length *x*

### Argument

Use the following argument to specify the value for which you want to compute the length using the `length` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| x | list, string, object | Specifies the value for which you want to compute the length. |

### Return value

The `length` function returns an integer representing the length of the specified list, string, or object.

### Examples

The following examples demonstrate the use of `length` in the Pact REPL:

```lisp
pact>(length [1 2 3])
3
```

In this example, the length of the list `[1, 2, 3]` is computed, resulting in 3.

```lisp
pact>(length "abcdefgh")
8
```

In this example, the length of the string `"abcdefgh"` is computed, resulting in 8.

```lisp
pact>(length { "a": 1, "b": 2 })
2
```

In this example, the length of the object `{ "a": 1, "b": 2 }` is computed, resulting in 2.
