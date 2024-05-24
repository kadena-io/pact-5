## contains

Use `contains` to test whether a `LIST` contains a `VALUE`, an `OBJECT` has a `KEY` entry, or a `STRING` contains a substring `VALUE`.

### Basic syntax

To test whether a `LIST` contains a `VALUE`, use the following syntax:

`(contains value list)`

To test whether an `OBJECT` has a `KEY` entry, use the following syntax:

`(contains key object)`

To test whether a `STRING` contains a substring `VALUE`, use the following syntax:

`(contains value string)`

### Arguments

Use the following arguments to specify the `VALUE`, `LIST`, `KEY`, `OBJECT`, `STRING` for the `contains` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `value` | `<a>` | Specifies the value to search for in a `LIST` or `STRING`. |
| `list` | `[<a>]` | Specifies the list in which to search for the `VALUE`. |
| `key` | `<a>` | Specifies the key to check for in an `OBJECT`. |
| `object` | `object:<{o}>` | Specifies the object in which to check for the `KEY`. |
| `string` | `string` | Specifies the string in which to search for the substring `VALUE`. |

### Return values

The `contains` function returns a boolean value indicating whether the `LIST` contains the `VALUE`, the `OBJECT` has the `KEY` entry, or the `STRING` contains the substring `VALUE`.

### Examples

The following examples demonstrate the `contains` function in the Pact REPL:

1. Testing if a `LIST` contains a `VALUE`:

```pact
pact>(contains 2 [1 2 3])
true
```

In this example, `contains 2 [1 2 3]` checks if the list `[1 2 3]` contains the value `2`, which it does, so it returns `true`.

2. Testing if an `OBJECT` has a `KEY` entry:

```pact
pact>(contains 'name { 'name: "Ted", 'age: 72 })
true
```

In this example, `contains 'name { 'name: "Ted", 'age: 72 }` checks if the object has the key `'name`, which it does, so it returns `true`.

3. Testing if a `STRING` contains a substring `VALUE`:

```pact
pact>(contains "foo" "foobar")
true
```

In this example, `contains "foo" "foobar"` checks if the string `"foobar"` contains the substring `"foo"`, which it does, so it returns `true`.
