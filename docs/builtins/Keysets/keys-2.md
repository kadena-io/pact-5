## keys-2

Use `keys-2` as a keyset predicate function to determine if there are at least two keys that match the keys defined in a keyset.

### Basic syntax

To check whether there are at least two keys that match the keys defined in a keyset, use the following syntax:

```pact
keys-2 count matched
```

### Arguments

Use the following arguments to specify the count of keys in the keyset and the number of matched keys using the `keys-2` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `count` | integer | Specifies the total count of keys defined in the keyset. |
| `matched` | integer | Specifies the number of matched keys. |

### Return value

The `keys-2` function returns a boolean value indicating whether there are at least two keys that match the keys defined in the keyset.

### Examples

The following example demonstrates how to use the `keys-2` to check if at least two keys are matched in a keyset where the total number of keys defined in a keyset is three and only one key is matched:

```pact
pact> (keys-2 3 1)
false
```

The function returns false because the condition of having at least two keys matched is not met.
