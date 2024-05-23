## at
Use `at` to index a `list` at a specified `idx` or to get the value with key `idx` from an `object`.

### Basic syntax

To index a `list` at a specified `idx` or get the value with key `idx` from an `object`, use the following syntax:

```lisp
(at idx list)
(at idx object)
```

### Arguments

Use the following arguments to specify the `idx` and `list` for indexing, or `idx` and `object` for key-based retrieval with the `at` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `idx` | `integer` | Specifies the index for the `list` or the key for the `object`. |
| `list` | `[<l>]` | Specifies the list from which to retrieve the value at `idx`. |
| `object` | `object:<{o}>` | Specifies the object from which to retrieve the value with key `idx`. |

### Return values

The `at` function returns the value at the specified `idx` in the `list` or the value with key `idx` from the `object`.

### Examples

The following example retrieves the value at index 1 from a list in the Pact REPL:

```lisp
pact>(at 1 [1 2 3])
2
```

In this example, `at` returns the value `2` from the list `[1 2 3]` at index 1.

The following example retrieves the value with key "bar" from an object in the Pact REPL:

```lisp
pact>(at "bar" { "foo": 1, "bar": 2 })
2
```

In this example, `at` returns the value `2` from the object `{ "foo": 1, "bar": 2 }` with the key "bar".
