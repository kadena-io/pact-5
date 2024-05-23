## remove
The `remove` function is used to remove an entry associated with a specified `KEY` from an `OBJECT`.

### Basic syntax

To remove an entry for a `KEY` from an `OBJECT`, use the following syntax:

`(remove KEY OBJECT)`

### Arguments

Use the following arguments to specify the `KEY` and `object` for removing an entry using the `remove` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `KEY` | `string` | Specifies the key for the entry to be removed from the object. |
| `OBJECT` | `object:<{o}>` | Specifies the object from which to remove the entry. |

### Return value

The `remove` function returns the modified object with the entry associated with the specified `KEY` removed.

### Example

The following example demonstrates the usage of the `remove` function within the Pact repl. It removes the entry for the `bar` key from the given object:

```lisp
pact>(remove "bar" { "foo": 1, "bar": 2 })
{ "foo": 1 }
```

This example illustrates how to use the `remove` function to remove a specific entry from an object in Pact.
