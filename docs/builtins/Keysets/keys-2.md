## keys-2

Use `keys-2` as a keyset predicate function to determine if at least two keys are matched in the keyset.

### Basic syntax

To use `keys-2` to check if at least two keys are matched in a keyset, use the following syntax:

keys-2 count matched

### Arguments

Use the following arguments to specify the count of keys in the keyset and the count of matched keys using the `keys-2` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `count` | `integer` | Specifies the total count of keys in the keyset. |
| `matched` | `integer` | Specifies the count of matched keys. |

### Return value

The `keys-2` function returns a boolean value indicating whether at least two keys are matched in the keyset.

### Examples

The following example demonstrates the use of `keys-2` in the Pact REPL:

```lisp
pact>(keys-2 3 1)
false
```

In this example, `keys-2` checks if at least two keys are matched in a keyset where the total count of keys is 3 and only 1 key is matched. The function returns false, indicating that the condition of having at least two keys matched is not met.
