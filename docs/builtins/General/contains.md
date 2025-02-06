## contains

Use `contains` to test whether a `list` contains a `value`, an `object` has a `key` entry, or a `string` contains a substring `value`.

### Basic syntax

Because `contains` is an overloaded function, there are a few different ways to use it.
To test whether a `list` contains a `value`, use the following syntax:

```pact
(contains value [list])
```

To test whether an `object` has a `key` entry, use the following syntax:

```pact
(contains key {object})
```

To test whether a `string` contains a substring `value`, use the following syntax:

```pact
(contains value string)
```

### Arguments

Use the following arguments to specify the `value`, `list`, `key`, `object`, `string` for the `contains` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `value` | any | Specifies the value to search for in a `list` or `string`. |
| `list` | [any] | Specifies the list to search for the specified `value`. |
| `key` | any | Specifies the key to check for in an `object`. |
| `object` | object | Specifies the object to check for the specified `key`. |
| `string` | string | Specifies the string to search for the specified `value`. |

### Return value

The `contains` function returns a boolean value indicating whether the specified `value` or the specified `key` was found.

### Examples

The following examples demonstrate the `contains` function in the Pact REPL.

To check whether a `list` contains a specified `value`:

```pact
pact> (contains 2 [1 2 3])
true
```

In this example, the `contains` functions checks whether the value `2` is included in the `[1 2 3]` list. Because the list contains the specified value, the `contains` function returns `true`.

To check whether a specified `object` has `'name` as a key field:

```pact
pact> (contains 'name { 'name: "Ted", 'age: 72 })
true
```

To check whether the string `"foobar"` contains the substring `"foo"`, 

```pact
pact> (contains "foo" "foobar")
true
```

In this example, `contains "foo" "foobar"` checks which it does, so it returns `true`.
