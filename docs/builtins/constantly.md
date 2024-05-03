Use `constantly` to lazily ignore the specified arguments *`IGNORE*` and return the specified *`VALUE`*.

### Basic syntax

To lazily ignore arguments and return a value, use the following syntax:

constantly *value* *ignore1* *ignore2* *ignore3* ...

### Arguments

Use the following arguments to specify the *`VALUE`* to return and the *`IGNORE*` arguments for the `constantly` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| value | <a> | Specifies the value to return. |
| ignore1, ignore2, ignore3, ... | <b>, <c>, <d>, ... | (Optional) Specifies the arguments to ignore. You can have multiple ignore arguments. |

### Return values

The `constantly` function returns the specified *`VALUE`*, ignoring any additional arguments.

### Examples

The following example demonstrates the `constantly` function in the Pact REPL:

```lisp
pact>(filter (constantly true) [1 2 3])
[1 2 3]
```

In this example, `constantly true` is used as the predicate function for `filter`. It ignores the elements of the list `[1 2 3]` and always returns `true`, so `filter` returns the entire list.

#### Additional Examples:

Here are a few more examples of using `constantly`:

```lisp
pact>(constantly "hello" 1)
"hello"

pact>(constantly "world" "ignore" "these" "arguments")
"world"

pact>(constantly 42 "ignore" 123 "arguments")
42
```

In each of these examples, `constantly` ignores the provided arguments and returns the specified value.
