## constantly

Use `constantly` to lazily ignore the specified `ignore` arguments and return the specified `vale`.

### Basic syntax

To lazily ignore arguments and return a value, use the following syntax:

```pact
(constantly value ignore1 ignore2 ...)
```

### Arguments

Use the following arguments to specify the `value` to return and the `ignore` arguments for the `constantly` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `value` | any | Specifies the value to return. |
| `ignore1, ignore2, ...` | any | (Optional) Specifies the arguments to ignore. You can have multiple ignore arguments. |

### Return values

The `constantly` function returns the specified `value`, ignoring any additional arguments.

### Examples

The following example demonstrates the `constantly` function in the Pact REPL:

```pact
pact> (filter (constantly true) [1 2 3])
[1 2 3]
```

In this example, `constantly true` is used as the predicate function for the `filter` function. 
The `constantly` function always returns `true` and ignores the elements of the `[1 2 3]` list argument, so the `filter` function always returns the entire list.

The following example illustrate specifying multiple arguments and argument using different data types:

```pact
pact> (constantly "hello" {"name": "Kris", "age": 45})
"hello"

pact> (constantly "world" 6 "ignore" "these" "arguments" [2.1 3.0])
"world"

pact> (constantly 42 "ignore" 123 "arguments")
42
```
