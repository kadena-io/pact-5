## remove

Use `remove` to remove an entry associated with a specified `key` from a specified `object`.

### Basic syntax

To remove an entry for a `key` from an `object`, use the following syntax:

```pact
(remove key object)
```

### Arguments

Use the following arguments to specify the `key` and `object` for removing an entry using the `remove` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `key` | string | Specifies the key for the entry to be removed from the object. |
| `object` | {object} | Specifies the object from which to remove the entry. |

### Return value

The `remove` function returns the modified object with the entry associated with the specified `key` removed.

### Example

The following example demonstrates how to use the `remove` function in the Pact REPL. 
This example removes the entry for the `bar` key from the given object:

```pact
pact> (remove "bar" { "foo": 1, "bar": 2 })
{ "foo": 1 }
```
