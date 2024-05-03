Use `keys-all` as a keyset predicate function to determine if all keys in the keyset are matched.

### Basic syntax

To use `keys-all` to check if all keys in a keyset are matched, use the following syntax:

keys-all *count matched*

### Arguments

Use the following arguments to specify the count of keys in the keyset and the count of matched keys using the `keys-all` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| count | integer | Specifies the total count of keys in the keyset. |
| matched | integer | Specifies the count of matched keys. |

### Return value

The `keys-all` function returns a boolean value indicating whether all keys in the keyset are matched.

### Examples

The following example demonstrates the use of `keys-all` in the Pact REPL:

```lisp
pact>(keys-all 3 3)
```

In this example, `keys-all` checks if all keys are matched in a keyset where the total count of keys is 3 and all 3 keys are matched. The function returns true, indicating that all keys in the keyset are matched.
