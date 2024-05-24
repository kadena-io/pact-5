## concat
Use `concat` to take a list of strings `str-list` and concatenate each of the strings in the list, returning the resulting string.

### Basic syntax

To concatenate each string in a list, use the following syntax:

`(concat str-list)`

### Arguments

Use the following argument to specify the list of strings `str-list` for the `concat` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `str-list` | `[string]` | Specifies the list of strings to concatenate. |

### Return values

The `concat` function returns the resulting string after concatenating all the strings in the `str-list`.

### Examples

The following example demonstrates the `concat` function in the Pact REPL:

```pact
pact>(concat ["k" "d" "a"])
"kda"
```

In this example, `concat` takes the list `["k" "d" "a"]` and concatenates each string in the list, resulting in the string `"kda"`.

The following example shows how to concatenate a list of strings after mapping each element with a function:

```pact
pact>(concat (map (+ " ") (str-to-list "abcde")))
"a b c d e"
```

In this example, `(str-to-list "abcde")` converts the string `"abcde"` into a list of characters `["a" "b" "c" "d" "e"]`. Then, `(map (+ " ") ...)` maps the function `(+ " ")` to each character, adding a space after each character. Finally, `concat` concatenates all the strings in the resulting list, producing the string `"a b c d e"`.
