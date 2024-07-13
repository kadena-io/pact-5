## keys-all

Use `keys-all` as a keyset predicate function to determine if all of the keys defined in the keyset are matched.

### Basic syntax

To check whether all of the keys defined in a keyset are matched, use the following syntax:

```pact
(keys-all count matched)
```

### Arguments

Use the following arguments to specify the count of keys in the keyset and the number of matched keys using the `keys-all` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `count` | integer | Specifies the total count of keys defined in the keyset. |
| `matched` | integer | Specifies the number of matched keys. |

### Return value

The `keys-all` function returns a boolean value indicating whether all keys in the keyset are matched.

### Examples

The following example demonstrates how to use the `keys-all` function to check whether all of the keys are matched in a keyset where the total number of keys defined is three:

```pact
pact> (keys-all 3 3)
true
```

The function returns true because  all keys in the keyset are matched.
