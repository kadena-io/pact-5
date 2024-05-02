The `remove` function is used to remove an entry associated with a specified *`key`* from an *`OBJECT`*.

## Basic syntax

To remove an entry for a *`key`* from an *`OBJECT`*, use the following syntax:

remove *key* *object*

## Arguments

Use the following arguments to specify the *`key`* and *`object`* for removing an entry using the `remove` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| key | string | Specifies the key for the entry to be removed from the object. |
| object | object:<{o}> | Specifies the object from which to remove the entry. |

## Return value

The `remove` function returns the modified object with the entry associated with the specified *`key`* removed.

## Example

The following example demonstrates the usage of the `remove` function within a Pact script. It removes the entry for the *`bar`* key from the given object:

```lisp
(remove "bar" { "foo": 1, "bar": 2 })
```

This example illustrates how to use the `remove` function to remove a specific entry from an object in Pact.
