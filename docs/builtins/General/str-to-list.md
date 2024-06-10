## str-to-list

Use `str-to-list` to convert a specified `string` into a list where each element is a single-character string.
Converting a string into a list can be useful for performing other more complex string manipulation tasks.

### Basic syntax

To convert a string into a list of single-character strings, use the following syntax:

```pact
(str-to-list string)
```

### Arguments

Use the following argument to specify the `string` to be converted into a list of single-character strings using the `str-to-list` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `string` | string | Specifies the string to be converted into a list. |

### Return value

The `str-to-list` function returns a list where each element represents a single character from the input `string`.

### Examples

The following examples demonstrate how to use the `str-to-list` function within the Pact REPL.

To convert the string "hello" into a list of single-character strings:

```pact
pact> (str-to-list "hello")
["h", "e", "l", "l", "o"]
```

To concatenate spaces between each character in the string "abcde":

```pact
(concat (map (+ " ") (str-to-list "abcde")))
" a b c d e"
```
