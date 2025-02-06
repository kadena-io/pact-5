## drop

Use `drop` to remove a specified number of values from a list, string, or object.

### Basic syntax

Because `drop` is an overloaded function, there are two ways to use it.
To remove the specified `count` number of values from a `list` or string, use the following syntax:

```pact
(drop count [list])
```

To remove the specified `keys` and corresponding values from an `object` or string, use the following syntax:

```pact
(drop keys {object})
```

### Arguments

Use the following arguments to specify the `count` and  `list` or `keys` and `object` for the `drop` Pact function:

| Argument | Type | Description                                                          |
|----------|------|----------------------------------------------------------------------|
| `count` | integer | Specifies the number of values to drop from the list or a string. |
| `list`  | [any] or string | Specifies the list (or string) from which to drop values. |
| `keys`  | [string] | Specifies the keys to drop from the object. |
| `object` | object | Specifies the object from which to drop entries with specified keys.|

### Return value

The `drop` function returns the modified list, string, or object after removing the specified number of values or specified keys.

### Examples

The following example demonstrates how to use the `drop` function to drop the first two characters from the specified string:

```pact
pact> (drop 2 "vwxyz")
"xyz"
```

The following example illustrates how to drop the last two values from the specified list:

```pact
pact> (drop -2 [1 2 3 4 5])
[1, 2, 3]

```

The following example drops the key `'name` and its corresponding value from the specified object:

```pact
pact> (drop ['name] { 'name: "Vlad", 'active: false })
{'active: false}
```
